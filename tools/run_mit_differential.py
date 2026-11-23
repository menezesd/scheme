#!/usr/bin/env python3
"""Compare vesper's semantics against MIT/GNU Scheme.

Each probe is evaluated three ways - vesper's bytecode VM, vesper's CPS
interpreter, and mit-scheme - and the three results must agree. Vesper models
MIT deliberately (see README), so a disagreement is either a vesper bug or a
place where the deviation should be recorded in EXPECTED_DEVIATIONS below.

Skips cleanly (exit 0) when mit-scheme is not installed, so `make test-diff`
stays usable on machines without it.
"""

import os
import math
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
VESPER = os.path.join(ROOT, "vesper")

PRELUDE = """\
(define (%probe thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (e) (k 'error)) thunk))))
(define (%emit thunk) (write (%probe thunk)) (newline))
"""

# Probes are single expressions. Each is wrapped in %probe so that an error is
# reported as the symbol error in every implementation rather than dropping
# one of them into a REPL.
PROBES = [
    # Initial ports retain their identity and participate in parameterize.
    "(list (eq? (current-input-port) (current-input-port)) (eq? (current-output-port) (current-output-port)) (eq? (current-error-port) (current-error-port)))",
    '(parameterize ((current-output-port (open-output-string))) (display "hello") (get-output-string (current-output-port)))',
    '(parameterize ((current-input-port (open-input-string "42"))) (read))',
    '(parameterize ((current-error-port (open-output-string))) (display "oops" (current-error-port)) (get-output-string (current-error-port)))',
    '(let ((old (current-output-port)) (p (open-output-string))) (dynamic-wind (lambda () #f) (lambda () (current-output-port p) (eq? (current-output-port) p)) (lambda () (set-current-output-port! old))))',
    '(let ((p (open-output-string))) (close-output-port p) (parameterize ((current-output-port p)) (eq? p (current-output-port))))',
    '(let ((p (open-input-string "x"))) (close-input-port p) (parameterize ((current-input-port p)) (eq? p (current-input-port))))',
    '(parameterize ((current-output-port 42)) #t)',
    '(parameterize ((current-output-port (open-input-string "x"))) #t)',
    '(let ((p (open-output-bytevector))) (parameterize ((current-output-port p)) (write-u8 65)) (get-output-bytevector p))',
    '(let ((p (open-output-bytevector))) (parameterize ((current-error-port p)) (write-u8 65 (current-error-port))) (get-output-bytevector p))',
    '(let ((p (open-output-string))) (display "λ𝄞" p) (close-output-port p) (close-output-port p) (get-output-string p))',
    '(call-with-output-string (lambda (p) (display "text" p) (close-output-port p)))',
    "(list 1/2@0 #x10@0 .5@0 1@-0.0)",
    '(map string->number \'("2@1" "1@1/2" "#e1@0" "#i2@0" "#x10@1" "1@2@3" "1@+i"))',
    "(list (make-polar 1/2 0) (make-polar +inf.0 0) (make-polar 0 1) (make-polar 0.0 -1) (make-polar 2 -0.0))",
    "(let ((r (expt 10 400))) (= (make-polar r 0) r))",
    "(let ((x '#1=(1 #1#))) (eq? (cadr x) x))",
    "(let ((x '#1=#2=#(#1#))) (eq? (vector-ref x 0) x))",
    "(let-syntax ((identity (syntax-rules () ((_ x) x)))) (let ((x (identity '#1=(a b))) (y (identity '#1#))) (eq? x y)))",
    "(let-syntax ((m (syntax-rules () ((m) '(|##protected##| . x))))) (m))",
    "(list (list= = '(2) '(1 . tail)) (list= = '() '(1 . tail)) (list= = '(2) '(1) 'tail))",
    "(let ((x (cons 1 'tail))) (list= = x x))",
    "(let ((x (circular-list 1 2))) (list= = x x))",
    "(list= = '(1 2) (circular-list 1 2))",
    "(let ((tail (list 2 3)) (count 0)) (list= (lambda (a b) (set! count (+ count 1)) (= a b)) (cons 1 tail) (cons 1 tail)) count)",
    "(eval '(parameterize (((make-parameter 1) 2)) 3) (environment '(scheme base)))",
    '(eval \'(guard (e (#t 42)) (error "imported guard")) (environment \'(scheme base)))',
    # User identifiers resembling implementation names still obey hygiene.
    "(let-syntax ((m (syntax-rules () ((m) |##gensym##999999|)))) (let ((|##gensym##999999| 7)) (m)))",
    "(let ((|##gensym##999998| 42)) (let-syntax ((m (syntax-rules () ((m) |##gensym##999998|)))) (let ((|##gensym##999998| 7)) (m))))",
    # A template's core keywords keep their meaning where the use site
    # rebinds them (they become core aliases).
    "(let-syntax ((m (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))) (let ((if list)) (m #f 3)))",
    "(let-syntax ((m (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))) (let ((let 0) (if list) (t 5)) (m #f t)))",
    "(let-syntax ((m (syntax-rules () ((_ a b) (begin a b))))) (let ((begin list)) (m 1 2)))",
    "(let-syntax ((m (syntax-rules () ((_ v) (cond (#f 1) (else v)))))) (let ((else #f)) (m 5)))",
    "(let-syntax ((m (syntax-rules () ((_) (quote (when 1 2)))))) (let ((quote list)) (m)))",
    "(let-syntax ((m (syntax-rules () ((_ v) ((lambda (y) y) v))))) (let ((lambda list)) (m 7)))",
    "(let-syntax ((m (syntax-rules () ((_ a) (lambda () a))))) ((lambda (x y) (let ((lambda list)) (list ((m x)) ((m y))))) 5 6))",
    "(let-syntax ((m (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))) (let ((set! list) (let 0) (p 1) (q 2)) (m p q) (list p q)))",
    "(let-syntax ((m (syntax-rules () ((_ n v) (define n v))))) (let ((define list)) (m x 9) x))",
    "(let-syntax ((m (syntax-rules () ((_ n) (letrec ((f (lambda (k) (if (= k 0) n (f (- k 1)))))) (f 3)))))) (let ((letrec list) (lambda list)) (m 8)))",
    "(let-syntax ((m (syntax-rules () ((_ x) (case x ((1) 'one) (else 0)))))) (let ((else #f)) (m 5)))",
    "(let-syntax ((m (syntax-rules () ((_ x) (cond ((assv x '((1 . one))) => cdr) (else 'none)))))) (let ((else #f) (=> #f)) (m 1)))",
    "(let-syntax ((m (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))) (let-syntax ((if (syntax-rules () ((_ a b c) 'macro-if)))) (m #f 3)))",
    "(let ((if list) (begin list) (letrec list) (lambda list)) (list (when #t 5) (do ((i 0 (+ i 1))) ((= i 3) i)) ((case-lambda ((a) a) ((a b) (+ a b))) 1 2)))",
    "(eval '(let ((else #f)) (case 1 (else 0))) (interaction-environment))",
    "(let-syntax ((quote (syntax-rules () ((quote x) 'macro-quote)))) (quote a))",
    # --- numeric tower ---
    "(+ 1 2)",
    "(exact->inexact 1/3)",
    "(- 1/2+3/4i)",
    "(/ 1/2+3/4i)",
    "(inexact->exact (make-rectangular 1/3 0.5))",
    "(inexact->exact (make-rectangular 0.5 1/3))",
    "(inexact->exact (make-rectangular 9007199254740993 0.5))",
    "(inexact->exact (make-rectangular +inf.0 1.0))",
    "(inexact->exact (make-rectangular 1.0 +inf.0))",
    "(expt 2 100)",
    "(expt 2 -1)",
    "(expt 1/2 3)",
    "(exact (/ 1 3))",
    "(round 2.5)",
    "(round 3.5)",
    "(round -2.5)",
    "(round 7/2)",
    "(floor -7/2)",
    "(ceiling -7/2)",
    "(truncate -7/2)",
    "(rationalize 1/1000 0)",
    "(rationalize -1/1000 -1/1000000)",
    "(rationalize 9007199254740993 0)",
    "(rationalize -3/2 1/2)",
    "(let ((z (make-rectangular (expt 10 400) 1))) (list (finite? z) (infinite? z) (nan? z)))",
    "(quotient -7 2)",
    "(remainder -7 2)",
    "(modulo -7 2)",
    "(gcd 0 0)",
    "(gcd 4 6)",
    "(lcm 4 6)",
    # inexactness contagion (R7RS 6.2.6)
    "(min 1 2.0)",
    "(max 1.0 2)",
    "(min 5 3 4.0)",
    "(min 1e300 1/2)",
    "(gcd 0.0 1)",
    "(gcd 4.0 6)",
    "(lcm 0.0 1)",
    "(lcm 4.0 6)",
    "(exact-integer? (round (max 1e-10 1.5 7/2)))",
    "(number->string 255 16)",
    "(number->string 1/3)",
    "(string->number \"#b101\")",
    "(string->number \"#o777\")",
    "(string->number \"#x-ff\")",
    "(string->number \"1/2\" 16)",
    "(= (real-part (string->number \"+inf.0+1.0i\")) +inf.0)",
    "(nan? (imag-part (string->number \"1+NaN.0i\")))",
    "(= (string->number \"+I\") 0+1i)",
    "(eqv? (string->number \"#e1.234567890123456789+1.0i\") 1234567890123456789/1000000000000000000+1i)",
    "(inexact? (real-part (string->number \"#i1+2i\")))",
    "(string->number \"#x1e-2i\")",
    "(string->number \"#e+inf.0+1i\")",
    "(read (open-input-string \"#e1.5+2.5i\"))",
    "(= (read (open-input-string \"#i+i\")) 0+1i)",
    "(read (open-input-string \"#d+inf.0\"))",
    "(read (open-input-string \".5+2i\"))",
    "(eqv? (string->number \"#i-0\") -0.0)",
    "(number->string 30-2i 16)",
    "(number->string 1/2+3/4i 2)",
    "(let* ((z (make-rectangular 1 (expt 10 400))) (s (number->string z))) (= z (string->number s)))",
    "(let ((s (number->string (make-rectangular 1.0 -0.0)))) (eqv? (imag-part (string->number s)) -0.0))",
    "(/ 1.0 0.0)",
    "(< 1 +inf.0)",
    "(sqrt 4)",
    "(sqrt 1/4)",
    "(sqrt -4)",
    "(log -1)",
    "(asin 2)",
    "(acos 2)",
    "(asin -2)",
    "(asin 0.5)",
    "(acos 0.5)",
    "(sin (asin 2))",
    "(atan 1 1)",
    "(atan 1+1i)",
    "(atan 0+2i)",
    "(imag-part (atan 0+1i))",
    "(imag-part (atan 0-1i))",
    "(imag-part (sqrt (make-rectangular -4.0 -0.0)))",
    "(sqrt 1.7e308+1.7e308i)",
    "(log 1.7e308+1.7e308i)",
    "(imag-part (asin 0.0+1e100i))",
    "(asin (make-rectangular 2.0 0.0))",
    "(acos (make-rectangular -2.0 -0.0))",
    "(sqrt1pm1 1.0+1e-20i)",
    "(sqrt1pm1 +inf.0)",
    "(log1p 1e-20+1e-20i)",
    "(log1p 0.0+1e-20i)",
    "(expm1 0.0+1e-20i)",
    "(log1p 1.7e308+1.7e308i)",
    "(= (expt 0.0+0.0i 2+1i) 0)",
    "(= (expt 0.0+0.0i 0) 1)",
    "(expt 0.0+0.0i -1)",
    "(imag-part (sqrt (exact->inexact (make-rectangular -4.0 -0.0))))",
    "(numerator 0.5)",
    "(denominator 0.5)",
    "(rationalize 3/10 1/10)",
    "(expt 2 1000)",
    "(* (expt 2 500) (expt 2 500))",
    "(gcd (expt 2 100) (expt 2 50))",
    "(quotient (expt 10 30) 7)",
    "(remainder (expt 10 30) 7)",
    "(eqv? 2 2.0)",
    "(eqv? 0.0 -0.0)",
    "(exact? 1/2)",
    "(integer? 2.0)",
    "(rational? 2.5)",

    # --- lists, strings, vectors, chars ---
    "(memq 2 '(1 2 . tail))",
    "(memv 2 '(1 2 . tail))",
    "(member 2 '(1 2 . tail))",
    "((member-procedure =) 2 '(1 2 . tail))",
    "(memq 2 '(1 . tail))",
    "(memv 2 '(1 . tail))",
    "(member 2 '(1 . tail))",
    "(list (memq 'c (list 'a 'b 'c 'd)) (memv 1.5 (list 1 1.5 2)) (member 2.0 '(1 2 3) =))",
    "(list (assq 'b '((a 1) (b 2))) (assv 2 '((1 . a) (2 . b))) (assoc 2.0 '((1 . a) (2 . b)) =))",
    "(assq 1 '((2 . 3) 5))",
    "(assv 1 '((2 . 3) . 4))",
    "(assoc 1 '(5))",
    "(list (caddr '(1 2 3)) (cdadr '(1 (2 3))) (cadddr '(1 2 3 4)) (cddddr '(1 2 3 4 5)) (map cadr '((1 2) (3 4))))",
    "(cadr '(1))",
    "(caddr 5)",
    "(cdddr '(1 2))",
    "(let ((calls 0)) (let ((result (call-with-values (lambda () (span (lambda (x) (set! calls (+ calls 1)) (< calls 3)) '(a b c d))) list))) (list result calls)))",
    "(let ((calls 0)) (let ((result (call-with-values (lambda () (break (lambda (x) (set! calls (+ calls 1)) (>= calls 3)) '(a b c d))) list))) (list result calls)))",
    "(find even? (circular-list 1 2 3))",
    "(any even? (circular-list 1 2 3))",
    "(every odd? (circular-list 1 2 3))",
    "(list-index even? (circular-list 1 2 3))",
    "(take-while odd? (circular-list 1 3 2))",
    "(let ((c (circular-list 1 3 2))) (eq? (drop-while odd? c) (cddr c)))",
    "(let ((c (circular-list 1 3 2))) (eq? (find-tail even? c) (cddr c)))",
    "(let ((c (circular-list 1 3 2))) (eq? (member 3 c) (cdr c)))",
    "(reverse '(1 2 3))",
    "(list-copy '(1 2 . 3))",
    "(last-pair '(1 2 3))",
    "(append '(1) '(2) 3)",
    "(concatenate '((1) (2 . tail)))",
    "(concatenate '(tail))",
    "(concatenate! (list (cons 1 'discarded) (cons 2 'tail)))",
    "(call-with-values (lambda () (unzip1 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip2 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip3 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip4 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip5 '((1 2 3 4 5 . tail)))) list)",
    "(assoc 2.0 '((1 a) (2 b)))",
    "(member 2.0 '(1 2 3))",
    "(assoc \"a\" '((\"a\" . 1)))",
    "(substring \"hello\" 1 3)",
    "(string-copy \"hello\" 1)",
    "(string-append \"a\" \"b\" \"c\")",
    "(string-upcase \"straße\")",
    "(string<? \"a\" \"b\" \"c\")",
    "(char-ci=? #\\A #\\a)",
    "(char->integer #\\newline)",
    "(equal? (vector 1 2) (vector 1 2))",
    "(vector->list (vector 1 2 3) 1)",
    "(string->list \"abc\" 1)",
    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 2 5) v)",
    "(apply + 1 2 '(3 4))",
    "(map + '(1 2) '(3 4))",
    "(map + '(1) '(4 . tail))",
    "(map + '(4 5 . tail) '(1))",
    "(filter-map + '(1) '(4 . tail))",
    "(count < '(1) '(4 . tail))",
    "(append-map list '(1) '(4 . tail))",
    "(append-map! list '(1) '(4 . tail))",
    "(fold + 0 '(1) '(4 5 . tail))",
    "(fold-right + 0 '(1) '(4 5 . tail))",
    "(fold + 0 '(1) '(4 . tail))",
    "(fold-right + 0 '(1) '(4 . tail))",
    "(pair-fold (lambda (a b acc) (+ (car a) (car b) acc)) 0 '(1) '(4 . tail))",
    "(pair-fold-right (lambda (a b acc) (+ (car a) (car b) acc)) 0 '(1) '(4 . tail))",
    "(let ((sum 0)) (for-each (lambda (a b) (set! sum (+ sum a b))) '(1) '(4 . tail)) sum)",
    "(let ((sum 0)) (pair-for-each (lambda (a b) (set! sum (+ sum (car a) (car b)))) '(1) '(4 . tail)) sum)",
    "(zip '(1 2 3) (circular-list 'a 'b))",
    "(map list '(1) '() 'unused)",
    "(count < '(1) '() 'unused)",
    "(pair-fold #f 9 '(1) '() 'unused)",
    "(zip '(1) '() 'unused)",
    "(let ((saved #f) (first #f) (again? #f)) (let ((result (map (lambda (x) (if (= x 2) (call-with-current-continuation (lambda (k) (set! saved k) x)) x)) '(1 2 3)))) (if again? (list first result) (begin (set! first result) (set! again? #t) (saved 20)))))",
    "(let ((saved #f) (first #f) (again? #f)) (let ((result (map (lambda (x y) (if (= x 2) (call-with-current-continuation (lambda (k) (set! saved k) (+ x y))) (+ x y))) '(1 2 3) '(10 20 30)))) (if again? (list first result) (begin (set! first result) (set! again? #t) (saved 200)))))",
    "(symbol->string 'Abc)",
    "(symbol->string (string->symbol \"Abc\"))",
    "(eq? (string->symbol \"abc\") 'abc)",
    "(eq? (string->symbol \"123\") (read (open-input-string \"|123|\")))",
    "(let () (define-record-type thing (make-thing x) thing? (x thing-x)) (let ((ctor make-thing)) (set! make-thing (lambda (x) x)) (thing? (ctor 2))))",
    "(let () (define-record-type thing (make-thing x) thing? (x thing-x set-thing-x!)) (let ((obj (make-thing 1))) (set! thing? (lambda (x) #f)) (set-thing-x! obj 2) (thing-x obj)))",
    "(let () (define-record-type thing (make-thing make-thing) thing? (make-thing thing-x)) (let ((obj (make-thing 1))) (list (thing? obj) (thing-x obj))))",
    # Records are opaque and disjoint from every other type. Probes compare
    # predicates and field values only: printed forms carry object-hash
    # numbers, which depend on everything printed before.
    "(let () (define-record-type point (make-point x y) point? (x point-x set-point-x!) (y point-y)) (let ((p (make-point 1 2))) (list (vector? p) (procedure? p) (record? p) (point? p) (point? (vector 1 2)) (eq? (record-type-descriptor p) point) (record-type? point) (record-type? p) (record? point) (pair? p) (string? p) (symbol? p) (number? p) (char? p) (boolean? p) (null? p) (bytevector? p) (port? p))))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (list (equal? (make-point 1 2) (make-point 1 2)) (let ((p (make-point 1 2))) (list (eqv? p p) (equal? p p) (eq? p p)))))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (point-x 5))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (vector-ref (make-point 1 2) 0))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (vector-length (make-point 1 2)))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (length (make-point 1 2)))",
    "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y set-point-y!)) (let ((p (make-point 1 2))) (set-point-y! p 10) (list (point-x p) (point-y p))))",
    "(let () (define-record-type point #f point? (x point-x)) (list (point? 5) (record-type? point)))",
    "(let () (define-record-type point make-point point? (x point-x) (y point-y)) (let ((p (make-point 1 2))) (list (point-x p) (point-y p))))",
    "(let () (define-record-type <pt> (make-pt a) pt? (a pt-a)) (list (record-type-name <pt>) (record-type-field-names <pt>) (record-type? <pt>) (record? <pt>)))",
    "(let () (define-record-type point (make-point point) point? (point point-x)) (list (point-x (make-point 7)) (record-type? point)))",
    "(let () (define-record-type node (make-node rtd layout index obj value) node? (rtd node-rtd) (layout node-layout) (index node-index) (obj node-obj) (value node-value set-node-value!)) (let ((n (make-node 1 2 3 4 5))) (set-node-value! n 50) (list (node-rtd n) (node-layout n) (node-index n) (node-obj n) (node-value n) (node? n))))",
    "(let () (define-record-type k (make-k) k?) (let ((t (make-equal-hash-table)) (a (make-k)) (b (make-k))) (hash-table-set! t a 1) (hash-table-set! t b 2) (list (hash-table-ref/default t a #f) (hash-table-ref/default t b #f) (hash-table-ref/default t (make-k) 'none))))",
    # MIT's procedural record interface.
    "(let* ((q (make-record-type 'q '(a b))) (make-q (record-constructor q)) (q? (record-predicate q)) (q-a (record-accessor q 'a)) (set-q-b! (record-modifier q 'b)) (qq (make-q 1 2))) (set-q-b! qq 5) (list (record-type-name q) (q? qq) (q-a qq) ((record-accessor q 'b) qq) (q? 5) (record? qq) (record-type-field-names q)))",
    "(let* ((q (make-record-type 'q '(a b))) (make-q2 (record-constructor q '(b))) (r (make-q2 9))) (list ((record-accessor q 'a) r) ((record-accessor q 'b) r)))",
    "(let* ((q (make-record-type 'q '(a b))) (make-q (record-constructor q))) (make-q 1))",
    "(let* ((q (make-record-type 'q '(a b)))) ((record-accessor q 'c) 1))",
    "(let* ((q (make-record-type 'q '(a b)))) ((record-accessor q 'a) 1))",
    "(let* ((q (make-record-type \"q\" '(a b)))) (record-type-name q))",
    "(record-type-descriptor (delay 1))",
    # The runtime's other opaque objects: conditions, promises, char-sets and
    # bytevector ports are not vectors; MIT's record? is true of all but
    # promises.
    "(list (record? 5) (record? (vector 1)) (record? (delay 1)) (record? (make-promise 1)) (record? (char-set #\\a)) (record? (open-input-bytevector (bytevector 1))) (record? (open-output-bytevector)) (record? 'a) (record? \"s\") (record? car) (record? (lambda () 1)))",
    "(list (vector? (delay 1)) (vector? (char-set #\\a)) (vector? (open-input-bytevector (bytevector 1))) (vector? (open-output-bytevector)))",
    "(list (promise? (delay 1)) (promise? (make-promise 1)) (promise? (vector 1)) (char-set? (char-set #\\a)) (char-set? (vector 'char-set 'ranges '())))",
    "(let ((p (open-input-bytevector (bytevector 1 2)))) (list (port? p) (input-port? p) (binary-port? p) (textual-port? p) (vector? p) (record? p)))",
    "(let ((p (open-output-bytevector))) (list (port? p) (output-port? p) (binary-port? p) (vector? p)))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (list (vector? e) (record? e) (error-object-message e) (error-object-irritants e) (procedure? e)))) (lambda () (error \"boom\" 1 2)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (error \"boom\" 1 \"two\" 'three)))))",

    # --- control, macros, exceptions ---
    "(let loop ((i 0) (a 0)) (if (= i 10) a (loop (+ i 1) (+ a i))))",
    "(do ((i 0 (+ i 1)) (a '() (cons i a))) ((= i 3) a))",
    "(case 2 ((1) 'one) ((2 3) 'two) (else 'other))",
    "(case 'a ((a) => (lambda (x) (list x x))) (else 'no))",
    "(case 'z ((a) 'no) (else => list))",
    "(cond ((assv 1 '((1 2))) => cadr) (else 'no))",
    "(call-with-current-continuation (lambda (k) (+ 1 (k 42))))",
    "(call-with-values (lambda () (values 1 2)) list)",
    "(let-values (((a b) (values 1 2))) (list a b))",
    "(dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))",
    "(call-with-current-continuation (lambda (k) (dynamic-wind (lambda () 'in) (lambda () (k 7)) (lambda () 'out))))",
    "(guard (e (#t (list 'caught e))) (raise 'boom))",
    "(guard (e ((symbol? e) 'sym)) (raise 'x))",
    "(guard (e (#t 'outer)) (guard (e ((string? e) 'nope)) (raise 'sym)))",
    "(with-exception-handler (lambda (e) 99) (lambda () (raise-continuable 'c)))",
    "(with-exception-handler (lambda (e) 99) (lambda () (+ 1 (raise-continuable 1))))",
    # A handler returning from a non-continuable raise must raise a secondary
    # exception that an enclosing guard can catch (R7RS 6.11), not abort.
    "(guard (e (#t 'caught)) (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x))))",
    "(call/cc (lambda (k) (with-exception-handler (lambda (e) (k 'outer)) (lambda () (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x)))))))",
    "(let ((x 'outer)) (define-syntax m (syntax-rules () ((_) x))) (let ((x 'inner)) (m)))",
    "(let ((tmp 1) (other 2)) (define-syntax sw (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (sw tmp other) (list tmp other))",
    "(let-syntax ((m (syntax-rules () ((_ x) (+ x 1))))) (m 5))",
    "(let ((case-key 5)) (case 1 ((1) case-key)))",
    "(let ((p (delay 1))) (list (force p) (force p)))",
    "(promise? (force (delay (delay 42))))",
    "(force (force (delay (delay 42))))",
    "(let ((first #t) (p #f)) (set! p (delay (if first (begin (set! first #f) (+ 1 (force p))) 2))) (list (force p) (force p)))",
    "(let ((count 0)) (let* ((p (delay (begin (set! count (+ count 1)) 42))) (q (delay-force p))) (force q) (force p) count))",
    "(let ((p (make-parameter 1))) (list (p) (parameterize ((p 2)) (p)) (p)))",
    "(let ((p (make-parameter 1)) (q (make-parameter 2))) (parameterize ((p 10) (q (p))) (list (p) (q))))",
    "(letrec ((f (lambda (x) (begin (begin (define (g) x))) (g)))) (f 42))",
    "(let-syntax ((h (syntax-rules () ((_) 99)))) (letrec ((f (lambda (x) (define (g) (h)) (begin (begin (define (h) x))) (g)))) (f 42)))",
    # procedure-arity: MIT's (min . max) pairs, max #f when unbounded. Vesper
    # takes a primitive's arity from primitive_table.c.
    "(map procedure-arity (list car cons + - * / = vector-ref number->string string-copy list append length call/cc call-with-values values string-append make-vector substring atan exact->inexact memq cadr vector-fill! eof-object bytevector-copy! map for-each max abs))",
    "(map procedure-arity (list (lambda () 1) (lambda (a b) a) (lambda (a . r) a) (lambda r r)))",
    "(let () (define (f a b c) a) (procedure-arity f))",
    "(procedure-arity 5)",
    # Ordinary non-tail recursion should not hit an engine limit at this depth.
    "(let ((f (lambda (f n) (if (= n 0) '() (cons n (f f (- n 1))))))) (length (f f 100000)))",
    # --- unassigned variables: a letrec, letrec* or internal-definition
    # variable read before its init has run is "Unassigned variable: NAME".
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x y) (y 1)) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (+ x 1))) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((v (vector 1 x)) (x 2)) v)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (lambda () y)) (y 1)) (x))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (if y 1 2)) (y #t)) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (letrec ((y z) (z 1)) y))) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x 1)) (letrec ((y x) (x 2)) y))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (begin (set! x 5) x))) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (begin (set! x 5) 7))) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (map (lambda (i) y) '(1 2))) (y 1)) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec ((x (apply (lambda () y) '())) (y 1)) x)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec* ((a (lambda () b)) (c (a)) (b 1)) c)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (letrec* ((a 1) (b (+ a 1))) b)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda () (define a (+ a 1)) a))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda () (define a 1) (define b (+ b a)) b))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda () (define (a) c) (define b (a)) (define c 2) b))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda () (define a (lambda () b)) (define c (a)) (define b 2) c))))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda (x) (define x (+ x 1)) x) 1)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda (x) (define (g) x) (define y (g)) (define x 10) y) 1)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () ((lambda (x) (define (g) x) (define x 10) (g)) 1)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (let () (define a 1) (define b (lambda () c)) (define c (b)) c)))))",
    "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i))))))",
]

def list_boundary_probes():
    """Exercise traversal boundaries without following an endless cycle.

    A successful search may stop before a dotted tail; indexed operations
    need only the requested prefix. Mutation cases receive fresh list copies.
    """
    probes = []
    for shape in ("'()", "'tail", "'(1 . tail)", "'(1 2 3 . tail)", "'(1 2 3)"):
        source = f"(list-copy {shape})"
        for proc in ("list-ref", "list-tail", "take", "drop", "take-right",
                     "drop-right", "take!", "drop-right!"):
            for count in (0, 1, 3, 4):
                probes.append(f"({proc} {source} {count})")
        for proc in ("split-at", "split-at!"):
            for count in (0, 1, 3, 4):
                probes.append(
                    f"(call-with-values (lambda () ({proc} {source} {count})) list)")
        for proc in ("find", "find-tail", "take-while", "drop-while",
                     "take-while!", "list-index", "any", "every"):
            for predicate in ("even?", "odd?"):
                probes.append(f"({proc} {predicate} {source})")
        for proc in ("span", "break", "span!", "break!", "partition", "partition!"):
            probes.append(
                f"(call-with-values (lambda () ({proc} even? {source})) list)")
        for proc in ("filter", "remove", "filter!", "remove!"):
            probes.append(f"({proc} even? {source})")
        for proc in ("length", "length+", "last-pair", "last", "reverse",
                     "reverse!", "list-copy"):
            probes.append(f"({proc} {source})")
    probes.extend([
        "(let ((c (circular-list 1 2 3))) (list-ref c 8))",
        "(let ((c (circular-list 1 2 3))) (eq? (list-tail c 8) (cddr c)))",
        "(let ((c (circular-list 1 2 3))) (list-set! c 7 9) (take c 3))",
        "(let ((c (circular-list 1 2 3))) (take! c 5))",
        "(let ((calls 0)) (let ((result (call-with-values (lambda () (partition! (lambda (x) (set! calls (+ calls 1)) (odd? calls)) (list 1 2 3))) list))) (list result calls)))",
    ])
    return probes


PROBES.extend(list_boundary_probes())


def mit_library_probes():
    """MIT's list, vector, stream, special-form, timing, trace and pp library.

    Streams are compared through their elements (MIT writes a stream as
    {a ...}); trace output contains MIT's object-hash numbers, so it is
    checked for its layout around them.
    """
    probes = [
        # fold-left, reduce-left, there-exists?, for-all?
        "(fold-left list '() '(1 2 3 4))",
        "(fold-left + 0 '())",
        "(fold-left cons* '() '(a b c) '(1 2 3))",
        "(fold-left list '() '(a b c) '(1 2) '(x y z))",
        "(fold-left - 0 '(1 2 3))",
        "(fold-left + 0 '(1 2 . 3))",
        "(fold-left + 0 'x)",
        "(fold-left 3 0 '())",
        "(fold-left 3 0 '(1))",
        "(fold-left list '() (circular-list 1 2) '(a b c))",
        "(fold-left list '() '(1 2) '(a b . c))",
        "(fold-left list '() '() 'x)",
        "(reduce-left list '() '(1 2 3 4 5))",
        "(list (reduce-left + 0 '()) (reduce-left + 0 '(5)) (reduce-left list 'z '(5)) (reduce-left - 0 '(10 2 3)))",
        "(reduce-left + 0 '(1 . 2))",
        "(reduce-left + 0 'x)",
        "(list (there-exists? '(1 2 3) even?) (there-exists? '(1 2 3) (lambda (x) (and (even? x) x))) (there-exists? '() even?) (there-exists? '(1 3) even?) (there-exists? '() 5))",
        "(list (for-all? '(2 4) even?) (for-all? '(2 4) (lambda (x) (and (even? x) x))) (for-all? '() even?) (for-all? '(2 3) even?))",
        "(there-exists? '(1 2 . 4) even?)",
        "(there-exists? '(1 3 . 4) even?)",
        "(for-all? '(2 4 . 5) even?)",
        "(for-all? 'x even?)",
        "(there-exists? '(1 2) 5)",
        # list-head, sublist, except-last-pair(!)
        "(list-head '(1 2) -1)",
        "(list-head '(1 2) 1.)",
        "(list-head '(1 2 3) 'x)",
        "(let ((l (list 1 2))) (eq? l (list-head l 2)))",
        "(sublist '(a b c . d) 1 3)",
        "(sublist '(a b c . d) 1 4)",
        "(sublist 'x 0 0)",
        "(sublist '(a b c) 1 'x)",
        "(sublist '(a b c) 1. 2)",
        "(let ((l (list 1 2 3))) (list (except-last-pair! l) l))",
        "(let ((l (list 1 2 3))) (list (except-last-pair l) l))",
        "(except-last-pair! (list 1))",
        "(except-last-pair! (cons 1 (cons 2 3)))",
        "(except-last-pair! (cons 1 2))",
        "(except-last-pair! '())",
        "(except-last-pair! 'x)",
        "(length (except-last-pair (iota 100000)))",
        "(length (list-head (iota 100000) 99999))",
        # make-initialized-list, list-of-type?, vector-of-type?
        "(make-initialized-list 4 (lambda (i) (* i i)))",
        "(make-initialized-list 0 5)",
        "(make-initialized-list -1 list)",
        "(make-initialized-list 3 5)",
        "(list (list-of-type? '(1 2 3) number?) (list-of-type? '(1 a 3) number?) (list-of-type? '(1 2 . 3) number?) (list-of-type? '() number?) (list-of-type? 5 number?))",
        "(list (vector-of-type? #(1 2) number?) (vector-of-type? #(1 a) number?) (vector-of-type? '(1) number?) (vector-of-type? #() number?))",
        # del-ass*
        "(del-assq 'b '((a . 1) (b . 2) (c . 3) (b . 4)))",
        "(del-assv 2 '((1 . a) (2 . b) (2 . c) (3 . d)))",
        "(del-assoc '(1) '(((1) . a) ((2) . b)))",
        "(del-assq 'a '((a . 1) (a 2) (b . 3)))",
        "(del-assq 'b '())",
        "(del-assq 'b 'x)",
        "(del-assq 'a '((a . 1) x))",
        "(del-assq 'a '((b . 1) 1))",
        "(del-assq 'a '((a . 1) (b . 2) . x))",
        "(let* ((l (list (list 'a 1) (list 'b 2))) (r (del-assq 'c l))) (list (eq? r l) (eq? (car r) (car l))))",
        "(let* ((l (list (cons 'a 1) (cons 'b 2) (cons 'a 3))) (r (del-assq! 'a l))) (list r l))",
        "(del-assv! 1 (list (cons 1 2)))",
        "(del-assoc! \"x\" (list (cons \"x\" 2) (cons \"y\" 3)))",
        "(del-assq! 'a 'x)",
        "(del-assq! 'a '(1))",
        # Vectors
        "(make-initialized-vector 4 (lambda (i) (* i i)))",
        "(make-initialized-vector 0 3)",
        "(make-initialized-vector -1 list)",
        "(make-initialized-vector 1. list)",
        "(make-initialized-vector 2 3)",
        "(let ((id (lambda (x) x))) (map (lambda (key) (vector-binary-search #(1 3 5 7) < id key)) '(0 1 3 4 5 7 8)))",
        "(vector-binary-search #() < (lambda (x) x) 7)",
        "(vector-binary-search #((1 . a) (3 . b) (5 . c)) < car 3)",
        "(vector-binary-search #((1 . a) (3 . b) (5 . c)) < car 4)",
        "(vector-binary-search (vector \"apple\" \"banana\" \"cherry\") string<? (lambda (x) x) \"banana\")",
        "(vector-binary-search #(5 5 5 5 5) < (lambda (x) x) 5)",
        "(let ((log '())) (vector-binary-search #(1 3 5 7 9 11 13) (lambda (a b) (set! log (cons (list a b) log)) (< a b)) (lambda (x) x) 9) (reverse log))",
        "(let ((log '())) (vector-binary-search #(1 3 5 7 9 11 13) (lambda (a b) (set! log (cons (list a b) log)) (< a b)) (lambda (x) x) 4) (reverse log))",
        "(let ((log '())) (vector-binary-search #(1 3 5 7 9 11 13 15) (lambda (a b) (set! log (cons (list a b) log)) (< a b)) (lambda (x) (set! log (cons (list 'u x) log)) x) 1) (reverse log))",
        "(vector-binary-search '(1 2) < (lambda (x) x) 5)",
        "(vector-binary-search #(1 3 5 7) 5 < (lambda (x) x))",
        "(vector-binary-search #(1 3 5 7) 5 -)",
        "(subvector '(a) 0 0)",
        "(subvector #(a b c) 1)",
        "(subvector #(1 2 3) 1. 2)",
        "(list (vector-head #(a b c d) 2) (vector-head #(a b c d) 0) (vector-tail #(a b c d) 2) (vector-tail #(a b c d) 4))",
        "(vector-head #(a b c d) 5)",
        "(vector-tail #(a b c d) 5)",
        "(vector-tail #(a b c d) -1)",
        "(vector-head '(1) 0)",
        "(vector-tail '(1) 0)",
        "(let ((g (vector-grow #(a b c) 5))) (list (vector-length g) (vector-head g 3)))",
        "(let ((v (vector 1 2))) (list (vector-grow v 2) (eq? v (vector-grow v 2))))",
        "(vector-grow (vector) 0)",
        "(vector-grow #(a b c) 2)",
        "(vector-grow '(a) 2)",
        "(vector-grow #(1 2) 'x)",
        "(vector-grow #(1 2) 2.)",
        "(vector-first #(1 2 3))",
        "(vector-first #())",
        "(subvector->list #(a b c d) 1 3)",
        "(subvector->list #(a b c d) 1 5)",
        "(subvector->list #(1 2 3) 2 1)",
        "(subvector->list '(1) 0 0)",
        "(let ((v (vector 1 2 3 4 5))) (subvector-fill! v 1 3 'x) v)",
        "(let ((v (vector 1 2 3 4 5))) (subvector-fill! v 1 6 'x) v)",
        "(subvector-fill! (vector 1 2) 1 0 'x)",
        "(subvector-fill! '(1 2) 0 0 'x)",
        "(let ((v (vector 1 2 3 4 5)) (w (vector 'a 'b 'c 'd 'e))) (subvector-move-left! v 1 3 w 2) w)",
        "(let ((v (vector 1 2 3 4 5)) (w (vector 'a 'b 'c 'd 'e))) (subvector-move-right! v 1 3 w 2) w)",
        "(let ((v (vector 1 2 3 4 5))) (subvector-move-left! v 1 4 v 0) v)",
        "(let ((v (vector 1 2 3 4 5))) (subvector-move-right! v 0 3 v 1) v)",
        "(let ((v (vector 1 2 3 4 5))) (subvector-move-left! v 0 3 v 1) v)",
        "(let ((v (vector 1 2 3))) (subvector-move-left! (vector 'a 'b) 2 2 v 3) v)",
        "(subvector-move-left! (vector 1 2) 0 3 (vector 1 2 3) 0)",
        "(subvector-move-left! (vector 1 2) 1 0 (vector 1 2 3) 0)",
        "(subvector-move-left! (vector 1 2) 0 2 (vector 1 2 3) 2)",
        "(subvector-move-left! (vector 1 2) 0 2 '(1 2 3) 0)",
        "(subvector-move-right! (vector 1 2) 0 2 (vector 1 2 3) -1)",
        # Streams
        "(let ((s (let ints ((n 0)) (cons-stream n (ints (+ n 1)))))) (list (stream-pair? s) (stream-car s) (stream-car (stream-cdr s)) (stream-first s) (stream-first (stream-rest s)) (stream-head s 5) (stream-car (stream-tail s 10)) (stream-ref s 7)))",
        "(list (stream-pair? (cons 1 2)) (stream-pair? (cons 1 (delay 2))) (stream-pair? (cons 1 (make-promise 2))) (stream-pair? '()) (stream-pair? 5) (stream-pair? (stream 1)))",
        "(stream-car '())",
        "(stream-cdr '())",
        "(stream-car (cons 1 2))",
        "(stream-cdr (cons 1 2))",
        "(stream-first '())",
        "(stream-rest '())",
        "(list (stream-null? '()) (stream-null? (stream 1)) (stream-null? 5) (empty-stream? '()) (empty-stream? the-empty-stream) the-empty-stream (stream) (list->stream '()) (stream-rest (stream 1)))",
        "(list (stream-head (stream 1 2 3) 2) (stream-head (stream) 0) (stream-head 5 0) (stream-tail (stream 1 2) 2) (stream-tail (stream) 0) (stream-tail 5 0))",
        "(stream-head (stream 1 2) 3)",
        "(stream-head (stream 1 2) -1)",
        "(stream-head (stream 1 2 3) 1.)",
        "(stream-head 5 1)",
        "(stream-tail (stream 1 2) 3)",
        "(stream-tail '(1 2) 1)",
        "(stream-ref (stream 1 2) 2)",
        "(stream-ref (stream 0 1 2) 1.)",
        "(stream-ref 5 0)",
        "(list (stream->list (stream 1 2 3)) (stream->list (stream)) (stream->list (list->stream '(a b c))) (stream->list (stream-tail (stream 1 2 3) 1)))",
        "(stream->list 5)",
        "(stream->list (cons 1 2))",
        "(stream->list (cons-stream 1 2))",
        "(list->stream 'x)",
        "(stream-car (list->stream '(a b . c)))",
        "(stream->list (list->stream '(a b . c)))",
        "(list (stream-length (stream 1 2 3)) (stream-length (stream)))",
        "(stream-length (cons-stream 1 2))",
        "(stream-length '(1 2))",
        "(stream-length (stream-head (stream 1 2) 1))",
        "(let ((n 0)) (let ((s (cons-stream 1 (begin (set! n (+ n 1)) '())))) (stream-cdr s) (stream-cdr s) n))",
        "(let ((n 0)) (cons-stream (begin (set! n (+ n 1)) 1) (set! n 10)) n)",
        "(let ((n 0)) (stream-head (cons-stream 1 (begin (set! n 1) '())) 1) n)",
        "(let ((n 0)) (stream-tail (cons-stream 1 (begin (set! n 1) '())) 1) n)",
        "(let ((n 0)) (stream-ref (cons-stream 1 (begin (set! n 1) '())) 0) n)",
        "(let ((n 0)) (stream-filter odd? (cons-stream 1 (begin (set! n 1) '()))) n)",
        "(let ((n 0)) (stream-map - (cons-stream 1 (begin (set! n 1) '()))) n)",
        "(let ((n 0)) (stream-append (cons-stream 1 (begin (set! n 1) '())) '()) n)",
        "(let ((s (let ints ((n 0)) (cons-stream n (ints (+ n 1)))))) (list (stream-head (stream-map + s s) 4) (stream-head (stream-filter even? s) 4) (stream-head (stream-filter odd? (stream-map (lambda (x) (* x 3)) s)) 3)))",
        "(list (stream->list (stream-map + (stream 1 2 3) (stream 10 20))) (stream->list (stream-map (lambda (x y) (list x y)) (stream 1 2 3) (stream 'a 'b 'c))) (stream-map car '()) (stream-filter car '()))",
        "(stream-map + '(1 2))",
        "(stream-head (stream-map + (stream 1 2 3)) 5)",
        "(list (stream->list (stream-filter even? (stream 1 2 3 4 5))) (stream-filter odd? (stream 2 4 6)) (stream-pair? (stream-filter odd? (stream 1 2 3))))",
        "(stream-filter even? '(1 2))",
        "(let ((acc '())) (stream-for-each (lambda (x) (set! acc (cons x acc))) (stream 1 2 3)) acc)",
        "(let ((acc '())) (stream-for-each (lambda (x y) (set! acc (cons (list x y) acc))) (stream 1 2 3) (stream 'a 'b)) (reverse acc))",
        "(stream-for-each display 5)",
        "(stream-for-each (lambda (x y) x) (stream 1 2) 5)",
        "(list (stream->list (stream-append (stream 1 2) (stream 3) (stream))) (stream->list (stream-append)) (stream-append 5) (stream-car (stream-append (stream 1) 5)))",
        "(let ((s (let ints ((n 0)) (cons-stream n (ints (+ n 1)))))) (stream-head (stream-append (stream 1 2) s) 4))",
        "(stream->list (stream-append (stream 1) 5))",
        "(stream-length (stream-append (stream 1) (cons-stream 2 3)))",
        "(list (stream->list (stream-append-map (lambda (x) (stream x x)) (stream 1 2 3))) (stream-append-map (lambda (x) (stream)) (stream 1 2)) (stream->list (stream-append-map (lambda (x y) (stream x y)) (stream 1 2) (stream 'a 'b 'c))))",
        "(stream-append-map (lambda (x) 5) (stream 1 2))",
        "(list (stream-accumulate + 0 (stream 1 2 3)) (stream-accumulate cons '() (stream 1 2 3)) (stream-accumulate list 'z (stream 1 2 3)))",
        "(stream-accumulate + 0 '(1))",
        "(list (stream-last (stream 1 2 3)) (stream->list (stream-last-pair (stream 1 2 3))))",
        "(stream-last (stream))",
        "(stream-last-pair (cons-stream 1 2))",
        "(let ((s (let ints ((n 0)) (cons-stream n (ints (+ n 1)))))) (list (stream->list (stream-truncate s (lambda (x) (> x 4)))) (stream->list (stream-truncate (stream 1 2 3) (lambda (x) #f)))))",
        "(list (call-with-output-string (lambda (p) (stream-write (stream 1 \"a\" 3) p))) (call-with-output-string (lambda (p) (stream-write (stream) p))) (call-with-output-string (lambda (p) (parameterize ((current-output-port p)) (stream-write (stream 'a \"b\"))))))",
        "(stream-write (stream 1 2) 5)",
        "(call-with-output-string (lambda (p) (stream-write (cons-stream 1 2) p)))",
        "(list (stream->list (cons-stream* 1 2 (stream 3 4))) (stream->list (cons-stream* (stream 3 4))) (stream-pair? (cons-stream* 1 2)) (cons-stream* 1))",
        "(stream-head prime-numbers-stream 10)",
        "(let ((s (let ints ((n 0)) (cons-stream n (ints (+ n 1)))))) (list (stream-ref s 20000) (length (stream-head s 20000))))",
        "(stream-accumulate + 0 (list->stream (iota 20000)))",
        # Special forms
        "((named-lambda (foo x) (* x 2)) 4)",
        "((named-lambda (foo x) (if (= x 0) 'done (foo (- x 1)))) 4)",
        "(let ((x 1)) (list (fluid-let ((x 2)) x) x))",
        "(let ((x 1) (y 2)) (fluid-let ((x 10) (y 20)) (set! x 11)) (list x y))",
        "(let ((x 1) (y 2)) (list (fluid-let ((x 10) (y x)) (list x y)) x y))",
        "(fluid-let () 5)",
        "(let ((x 1)) (list (call-with-current-continuation (lambda (k) (fluid-let ((x 2)) (k x)))) x))",
        "(let ((x 10) (k #f) (n 0) (log '())) (fluid-let ((x 30)) (call/cc (lambda (c) (set! k c))) (set! log (cons x log)) (set! x (+ x 1))) (set! log (cons x log)) (if (< n 1) (begin (set! n (+ n 1)) (k #f))) (reverse log))",
        "(let () (define-integrable (sq x) (* x x)) (sq 5))",
        "(begin (assert #t) (assert 1) (assert '()) 'ok)",
        "(assert #f)",
        "(let ((x 5)) (assert (> x 3) \"x too small\") 'ok)",
        "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (assert (= 1 2))))))",
        "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (assert #f (string-append \"a\" \"b\") 'x)))))",
        # Miscellaneous
        "(list (call-with-output-string (lambda (p) (write-line '(1 \"a\" #\\b) p))) (call-with-output-string (lambda (p) (parameterize ((current-output-port p)) (write-line \"x\") (write-line 'y)))))",
        "(write-line \"a\" 5)",
        "(list (symbol-append 'foo 'bar) (symbol-append) (symbol-append 'a \"b\") (symbol-append 'a 1) (symbol-append 'a 'b 'c))",
        "(list (symbol<? 'a 'b) (symbol<? 'b 'a) (symbol<? 'a 'a) (symbol<? 'abc 'abd) (symbol<? 'b 'abc))",
        "(symbol<? 'a \"b\")",
        "(symbol<? 'a 'b 'c)",
        "(list (char-ascii? #\\a) (char-ascii? #\\x7f) (char-ascii? #\\x80) (char-ascii? #\\x100))",
        "(char-ascii? 1)",
        "(list (cube 3) (cube 2/3) (cube 1.5))",
        "(cube 'x)",
        "(with-values (lambda () (values 1 2)) list)",
        "(let ((k (constant-procedure 5))) (list (k) (k 1 2 3)))",
        "(list (guarantee integer? 5 'foo) (guarantee (lambda (x) (and (integer? x) 'yes)) 5 'foo))",
        "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (guarantee integer? 'x 'foo)))))",
        "(call-with-current-continuation (lambda (k) (with-exception-handler (lambda (e) (k (condition/report-string e))) (lambda () (guarantee integer? 'x)))))",
        "(let ((h (make-equal-hash-table))) (hash-table/put! h 'a 1) (let* ((a (hash-table/get h 'a #f)) (b (hash-table/get h 'b 'none)) (n (hash-table/count h))) (hash-table/remove! h 'a) (hash-table/put! h 'x 1) (let ((keys (hash-table/key-list h)) (data (hash-table/datum-list h))) (hash-table/clear! h) (list a b n keys data (hash-table/count h)))))",
        "(hash-table/get (make-equal-hash-table) 'a)",
        "(hash-table/get 5 'a #f)",
        # Timing: only the deterministic properties
        "(list (inexact? (runtime)) (exact-nonnegative-integer? (real-time-clock)) (exact-nonnegative-integer? (process-time-clock)) (internal-time/ticks->seconds 1500) (internal-time/seconds->ticks 1.5))",
        "(with-timings (lambda () 'done) (lambda (run gc real) 'ignored))",
        "(let ((got #f)) (with-timings (lambda () 1) (lambda (run gc real) (set! got (map exact-nonnegative-integer? (list run gc real))))) got)",
        # trace: checked around MIT's hash numbers
        "(let ((p (open-output-string))) (define (f x) (* x 2)) (trace f) (let ((r (parameterize ((trace-output-port p)) (f 3)))) (untrace f) (let ((s (get-output-string p))) (list r (string-search-forward \"[Entering #[compound-procedure \" s 0) (and (string-search-forward \" f]\\n    Args: 3]\\n[6\\n      <== #[compound-procedure \" s 0) #t) (and (string-search-forward \" f]\\n    Args: 3]\\n\" (substring s 40 (string-length s)) 0) #t)))))",
        "(let ((p (open-output-string))) (define (h x) (if (> x 0) (h (- x 1)) 'done)) (trace h) (parameterize ((trace-output-port p)) (h 2)) (untrace h) (let ((s (get-output-string p))) (list (length (filter (lambda (c) (char=? c #\\newline)) (string->list s))) (and (string-search-forward \"Args: 0]\\n[done\\n\" s 0) #t))))",
        "(let ((p (open-output-string))) (define (g a . rest) (list a rest)) (trace-entry g) (parameterize ((trace-output-port p)) (g 1 2 3) (g 1)) (untrace g) (let ((s (get-output-string p))) (list (and (string-search-forward \" g]\\n    Args: 1\\n          (2 3)]\\n\" s 0) #t) (and (string-search-forward \" g]\\n    Args: 1\\n          ()]\\n\" s 0) #t) (string-search-forward \"<==\" s 0))))",
        "(let ((p (open-output-string))) (define (g a . rest) (list a rest)) (trace-exit g) (parameterize ((trace-output-port p)) (g 1 2)) (untrace g) (let ((s (get-output-string p))) (list (string-search-forward \"[(1 (2))\\n      <== #[compound-procedure \" s 0) (string-search-forward \"Entering\" s 0))))",
        "(let ((p (open-output-string))) (define (k) 'k) (trace k) (parameterize ((trace-output-port p)) (k)) (untrace k) (let ((s (get-output-string p))) (list (and (string-search-forward \" k]]\\n[k\\n      <== #[compound-procedure \" s 0) #t))))",
        "(let ((p (open-output-string))) (define (big) (iota 30)) (trace big) (parameterize ((trace-output-port p)) (big)) (untrace big) (let ((s (get-output-string p))) (and (string-search-forward \" big]]\\n[(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28...\\n      <== \" s 0) #t)))",
        "(let ((p (open-output-string))) (define (str s) s) (trace str) (parameterize ((trace-output-port p)) (str (make-string 70 #\\a)) (str (make-string 76 #\\b))) (untrace str) (let ((s (get-output-string p))) (list (and (string-search-forward (string-append \"    Args: \\\"\" (make-string 67 #\\a) \"...]\") s 0) #t) (and (string-search-forward (string-append \"[\\\"\" (make-string 70 #\\a) \"\\\"\\n\") s 0) #t) (and (string-search-forward (string-append \"[\\\"\" (make-string 76 #\\b) \"...\\n\") s 0) #t))))",
        "(let ((p (open-output-string))) (define (f x) x) (trace-both f) (trace-entry f) (parameterize ((trace-output-port p)) (f 4)) (untrace-exit f) (parameterize ((trace-output-port p)) (f 5)) (untrace-entry f) (parameterize ((trace-output-port p)) (f 6)) (let ((s (get-output-string p))) (list (and (string-search-forward \"Args: 4]\\n[4\\n\" s 0) #t) (and (string-search-forward \"Args: 5]\\n\" s 0) #t) (string-search-forward \"[5\" s 0) (string-search-forward \"Args: 6\" s 0))))",
        "(let ((p (open-output-string))) (define (thrower x) (car x)) (trace thrower) (parameterize ((trace-output-port p)) (call/cc (lambda (k) (with-exception-handler (lambda (e) (k #f)) (lambda () (thrower 5)))))) (untrace thrower) (let ((s (get-output-string p))) (list (and (string-search-forward \"Entering\" s 0) #t) (string-search-forward \"<==\" s 0))))",
        "(let () (define (f) 1) (untrace f))",
        "(let ((five 5)) (trace five))",
        # pp: layout as MIT prints it
    ]
    pp_cases = [
        "'(if a b c)", "'(1 (if a b c))", "'(cond (a b) (else c))", "'(and a b)",
        "'(or a b)", "'(let ((x 1)) x)", "'(let* ((x 1)) x)", "'(letrec ((x 1)) x)",
        "'(let loop ((x 1)) x)", "'(define x 1)", "'(define-syntax a b)",
        "'(define-record-type p (mk) p?)", "'(named-lambda (f) 1)", "'(lambda () 1)",
        "'(lambda (x))", "'(begin a b)", "'(do ((i 0)) (#t) x)", "'(case x (else 1))",
        "'(when a b)", "'(fluid-let ((x 1)) x)", "'(let-values (((a) 1)) a)",
        "'(parameterize ((p 1)) 1)", "'(guard (e (#t 1)) 1)", "'(set! x 1)",
        "'(let* a b)", "'(let a b)", "'(fluid-let a b)", "'(define* a b)",
        "'(define-library a b)", "'(if)", "'(if a)", "'(let x)", "'(f)",
        "''a", "'(quote a b)", "'(1 'a `b ,c ,@d)", "'(a . b)", "'(a b . c)",
        "'#(a #(b c))", "(list->vector (iota 3))", "(vector)", "\"a\\nb\"",
        "(list 'a (string->symbol \"B c\") \"s\\\\t\" #\\space 1/2 #t '())",
        "'(define (long-function-name-here alpha beta gamma) (let ((delta (+ alpha beta)) (epsilon (* gamma gamma))) (if (> delta epsilon) (list 'delta-wins delta epsilon) (begin (display \"epsilon wins\") (newline) (list 'epsilon-wins epsilon delta)))))",
        "'(aaaaaaaaaa bbbbbbbbbbb ccccccccccc ddddddddddd eeeeeeeeeee fffffffffff ggggggggggg hhhhhhhhhhh iiiiiiiiii)",
        "'((aaaaaaaaaa . 1) (bbbbbbbbbbb . 2) (ccccccccccc . 3) (ddddddddddd . 4) (eeeeeeeeeee . 5) (fffffffffff . 6))",
        "(iota 40)", "(make-vector 30 'abc)", "(list->vector (iota 30))",
        "(list->vector (iota 100))",
        "'(foo (bar baz quux quuux quuuux quuuuux) (bar baz quux quuux quuuux quuuuux) (bar baz quux quuux quuuux quuuuux))",
        "'(let loop ((i 0) (acc '())) (if (= i 10) (reverse acc) (loop (+ i 1) (cons (* i i) acc))))",
        "'(cond ((assv key alist) => cdr) ((eq? key 'default) default-value-here) (else (error \"Unknown key:\" key)))",
        "'(if (and (pair? some-long-variable) (null? (cdr some-long-variable))) (car some-long-variable) (error \"bad\"))",
        "'(do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 100000000000) (reverse acc)) (display i) (newline))",
        "'(case x ((1 2 3) 'small-number-here) ((4 5 6) 'medium-number-here) (else 'large-number-here))",
        "'(f (g (h (i (j (k (l (m (n (o (p (q (r (s (t (u (v (w (x (y (z 1234567890123456789)))))))))))))))))))))",
        "'(define-syntax swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))",
        "'(set! some-variable (some-function-with-a-long-name argument-one argument-two arg-three))",
        "'#(aaaaaaaaaaa bbbbbbbbbbbb ccccccccccccc dddddddddddd eeeeeeeeeeeee ffffffffffff ggggggggg)",
        "(list 'ff 'g (make-string 69 #\\a))", "(list 'ff 'g (make-string 70 #\\a))",
        "(list 'ff 'g (make-string 72 #\\a))", "(list 'ff (make-string 72 #\\a) 'g)",
        "(list 'ff (make-string 73 #\\a) 'g)", "(list (make-string 75 #\\a) 'b)",
        "(list 'f (list (make-string 70 #\\a)))",
        "(cons 'f (cons (make-string 40 #\\a) (make-string 40 #\\b)))",
        "(vector 1 2 3 (make-string 80 #\\a))", "(list (vector 1 2 3 (make-string 80 #\\a)))",
        "(list 'f (list 'quote (list (make-string 40 #\\a) (make-string 40 #\\b))))",
        "'(let ((aaaaaaaaaaaaaaaa 1111111111111) (bbbbbbbbbbbbbbbbbbbbbb 2222222222222) (cccccccccccccccccc 33333333333333)) x)",
        "'(let loop ((aaaaaaaaaaaaaaaa 1111111111111) (bbbbbbbbbbbbbbbbbbbbbb 2222222222222) (cccccccccccccccccc 33333333333333)) x)",
        "'(let loop ((aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 1111111111111111111111111111111111111) (bbbbbbbbbbbbbbbbbbbbbb 2222222222222) (cccccccccccccccccc 33333333333333)) x)",
        "'(case (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa x) ((a) 1) (else 2))",
        "'(define (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccc) 1)",
        "''(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33)",
        "''(aaaaaaaaaaaaaa (if a b c) cccccccccccccccccccc ddddddddddddddd eeeeeeeeeeeeeeeeeeeeeeeeee)",
        "(vector (make-string 40 #\\a) 1 2 3 (make-string 40 #\\b))",
        "(list 'a (list 'b) (list 'c (list 'd (list 'e (list 'f (make-string 60 #\\x))))))",
        "(vector 'a (vector 'b) (list 'c (list 'd (list 'e (list 'f (make-string 60 #\\x))))))",
    ]
    probes.extend("(call-with-output-string (lambda (port) (pp %s port)))" % case
                  for case in pp_cases)
    probes.extend([
        "(call-with-output-string (lambda (port) (pp '(if a b c) port #f)))",
        "(call-with-output-string (lambda (port) (pp '(1 2 3 'a) port #f)))",
        "(call-with-output-string (lambda (port) (pp '(aaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbb cccccccccccccccccccc ddddddddddddddd eeeeeeeeeeeeeeeeee) port #f)))",
        "(call-with-output-string (lambda (port) (pp '(if a b c) port #t 4)))",
        "(call-with-output-string (lambda (port) (parameterize ((current-output-port port)) (pp '(a b)))))",
        "(let () (define-record-type point (make-point x y) point? (x point-x) (y point-y)) (let ((s (call-with-output-string (lambda (port) (pp (make-point 1 '(a b)) port))))) (substring s (string-search-forward \"]\" s 0) (string-length s))))",
        "(pp '(a) 5)",
    ])
    return probes


PROBES.extend(mit_library_probes())


# Probes where vesper deliberately differs from MIT. Keep this list short and
# justified; each entry is (probe, reason).
EXPECTED_DEVIATIONS = {
    # MIT's vector-copy! copies backwards, which corrupts this overlap. R7RS
    # requires the result to be as if the source were copied first.
    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 2 5) v)":
        "MIT copies overlapping ranges backwards; R7RS requires copy-first semantics",
}


DATUM_TOKEN = re.compile(
    r'#\\(?:[^\s()]+|.)|"(?:\\.|[^"\\])*"|\|(?:\\.|[^|\\])*\|'
    r'|[()]|\s+|[^()\s]+')
NUMERIC_START = re.compile(
    r'^[+-]?(?:[0-9]|\.[0-9])|^[+-](?:inf\.0|nan\.0|i$)', re.IGNORECASE)


def normalize_number(text):
    text = text.lower()
    # MIT writes .5 and 1. where vesper writes 0.5 and 1.0
    text = re.sub(r"(?<![0-9a-zA-Z_.])(-?)\.([0-9])", r"\g<1>0.\g<2>", text)
    text = re.sub(r"([0-9])\.(?![0-9])", r"\g<1>.0", text)
    # exponent shape: 1e+21 / 1e-07 vs 1e21 / 1e-7
    text = re.sub(r"e\+?(-?)0*(\d)", r"e\g<1>\g<2>", text)
    # MIT drops a zero real part from a complex number: +2i vs 0+2i
    text = re.sub(r"(?<![0-9a-zA-Z_.])0\.?0*\+", "+", text)
    return text


def normalize(text):
    """Normalize number spelling while preserving literal data and symbols."""
    return "".join(normalize_number(token) if NUMERIC_START.match(token) else token
                   for token in DATUM_TOKEN.findall(text.strip()))


NUMBER = re.compile(r"-?\d+\.\d+(?:e-?\d+)?")


def equivalent(a, b):
    """Equal after normalization, allowing last-ULP drift in inexact numbers.

    Vesper and MIT reach the same inexact value by different routes (asin/acos
    via the complex logarithm, say), so results can differ in the final digit.
    Compare the non-numeric skeleton exactly and the floats with a relative
    tolerance.
    """
    if a == b:
        return True
    left = DATUM_TOKEN.findall(a)
    right = DATUM_TOKEN.findall(b)
    if len(left) != len(right):
        return False
    for first, second in zip(left, right):
        if first == second:
            continue
        if not (NUMERIC_START.match(first) and NUMERIC_START.match(second)):
            return False
        if NUMBER.sub("#", first) != NUMBER.sub("#", second):
            return False
        for x, y in zip(NUMBER.findall(first), NUMBER.findall(second)):
            x, y = float(x), float(y)
            if x == y == 0 and math.copysign(1, x) != math.copysign(1, y):
                return False
            if not math.isclose(x, y, rel_tol=1e-12, abs_tol=0):
                return False
    return True


def build_source(probes):
    """One program that prints one line per probe.

    Running every probe in a single process keeps this usable inside
    `make test-all`: mit-scheme takes about a second to start, so a process
    per probe per implementation costs minutes. %probe contains errors, so one
    failing probe cannot swallow the ones after it.
    """
    body = "".join("(%%emit (lambda () %s))\n" % probe for probe in probes)
    return PRELUDE + body


# Whole programs, for what a probe cannot do in its shared top level: each
# runs on its own, printing one line per %emit, in all three systems. A core
# keyword redefined at top level reaches code written afterwards, and a user
# macro defined before (it shares the frame), but not procedures written
# before nor the standard library's macros.
PROGRAMS = [
    """(define (g x) (if x 1 2))
(define-syntax m (syntax-rules () ((_ x) (if x 1 2))))
(define if list)
(%emit (lambda () (g #t)))
(%emit (lambda () (m #t)))
(%emit (lambda () (if 1 2 3)))
(%emit (lambda () (when #t 7)))
(%emit (lambda () (map (lambda (x) (* x x)) '(1 2 3))))
(%emit (lambda () (call-with-output-string (lambda (p) (display 42 p)))))
""",
    """(define (if a b c) (list a b c))
(%emit (lambda () (if 1 2 3)))
(%emit (lambda () (map (lambda (x) (+ x 1)) '(1 2 3))))
(%emit (lambda () (assq 'b '((a 1) (b 2)))))
(%emit (lambda () (call-with-output-string (lambda (p) (write 'ok p)))))
""",
    """(define if 5)
(%emit (lambda () (if #t 1 2)))
(%emit (lambda () (call-with-output-string (lambda (p) (write 'still-printing p)))))
(%emit (lambda () (unless #f 'unless-works)))
""",
    # The stdlib's case keeps matching its else and => literals after a
    # top-level definition of those names (a binding comparison, as in MIT).
    """(define else #f)
(%emit (lambda () (case 3 ((1) 'a) (else 'b))))
(define => #f)
(%emit (lambda () (case 2 ((2) => (lambda (x) (* x 3))) (else 0))))
""",
]


def run_scheme(argv_for, source, timeout=600):
    handle, path = tempfile.mkstemp(suffix=".scm")
    try:
        with os.fdopen(handle, "w") as f:
            f.write(source)
        try:
            # stdin must be closed: mit-scheme drops into its REPL after
            # --load and would otherwise wait on an inherited stdin forever.
            done = subprocess.run(argv_for(path), capture_output=True,
                                  text=True, timeout=timeout,
                                  stdin=subprocess.DEVNULL)
        except subprocess.TimeoutExpired:
            raise RuntimeError(f"timed out after {timeout}s") from None
        if done.returncode != 0:
            detail = done.stderr.strip()
            raise RuntimeError(
                f"exited with status {done.returncode}"
                + (f"\n{detail}" if detail else ""))
        out = done.stdout
        # vesper's --interpreter banner is not part of the result
        return "\n".join(line for line in out.split("\n")
                         if not line.startswith("; CPS interpreter"))
    finally:
        os.unlink(path)


def main():
    mit = shutil.which("mit-scheme")
    if not mit:
        print("SKIP: mit-scheme not installed; differential probes not run")
        return 0
    if not os.path.exists(VESPER):
        print("FAIL: %s not built" % VESPER)
        return 1

    source = build_source(PROBES)
    engines = [
        ("vm", lambda path: [VESPER, path]),
        ("cps", lambda path: [VESPER, "--interpreter", path]),
        ("mit", lambda path: [mit, "--quiet", "--load", path, "--eval", "(exit)"]),
    ]

    results = {}
    for name, argv_for in engines:
        try:
            output = run_scheme(argv_for, source)
        except (OSError, RuntimeError) as error:
            print(f"FAIL: {name}: {error}")
            return 1
        lines = [normalize(line) for line in output.splitlines()]
        if len(lines) != len(PROBES):
            print("FAIL: %s produced %d result lines for %d probes"
                  % (name, len(lines), len(PROBES)))
            if len(lines) < len(PROBES):
                print("      stopped at probe: %s" % PROBES[len(lines)])
            return 1
        results[name] = lines

    failures = []
    deviations = 0
    for i, probe in enumerate(PROBES):
        vm, cps, ref = results["vm"][i], results["cps"][i], results["mit"][i]
        if not equivalent(vm, cps):
            failures.append((probe, "VM and CPS interpreter disagree", vm, cps, ref))
            continue
        if probe in EXPECTED_DEVIATIONS:
            if equivalent(vm, ref):
                failures.append((probe, "listed as an expected deviation but "
                                 "now agrees with MIT", vm, cps, ref))
            else:
                deviations += 1
        elif not equivalent(vm, ref):
            failures.append((probe, "vesper and MIT disagree", vm, cps, ref))

    for program in PROGRAMS:
        outputs = {}
        for name, argv_for in engines:
            try:
                # A program that loops forever is the failure this catches.
                outputs[name] = run_scheme(argv_for, PRELUDE + program,
                                           timeout=60)
            except (OSError, RuntimeError) as error:
                outputs[name] = f"<{error}>"
        vm, cps, ref = (normalize(outputs[n]) for n in ("vm", "cps", "mit"))
        label = program.strip().replace("\n", " ")
        if not equivalent(vm, cps):
            failures.append((label, "VM and CPS interpreter disagree", vm, cps, ref))
        elif not equivalent(vm, ref):
            failures.append((label, "vesper and MIT disagree", vm, cps, ref))

    for probe, why, vm, cps, ref in failures:
        print("FAIL: %s" % probe)
        print("      %s" % why)
        print("      vm  : %s" % vm)
        print("      cps : %s" % cps)
        print("      mit : %s" % ref)
    # An entry that matches no probe exempts nothing and hides a typo.
    unused = [probe for probe in EXPECTED_DEVIATIONS if probe not in PROBES]
    for probe in unused:
        print("FAIL: expected deviation is not a probe: %s" % probe)

    print("%d probes, %d programs, %d failures (%d expected deviations skipped)"
          % (len(PROBES), len(PROGRAMS), len(failures) + len(unused), deviations))
    return 1 if failures or unused else 0


if __name__ == "__main__":
    sys.exit(main())
