;;; R5RS Conformance Tests
;;; Tests core R5RS features for compliance
;; This file is intentionally loaded as a large source file; its macro and
;; continuation sections also exercise streaming top-level loading.

;; Test framework
(define test-pass 0)
(define test-fail 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! test-pass (+ test-pass 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! test-fail (+ test-fail 1))
        (display "  FAIL: ")
        (display name)
        (display " - expected ")
        (write expected)
        (display " got ")
        (write actual)
        (newline))))

(define (test-section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ============================================================================
;;; 4.1 Primitive expression types
;;; ============================================================================

(test-section "4.1.1 Variable references")
(define x 28)
(test "variable reference" 28 x)

(test-section "4.1.2 Literal expressions")
(test "quote symbol" 'a (quote a))
(test "quote list" '(+ 1 2) (quote (+ 1 2)))
(test "quote vector" '#(a b c) (quote #(a b c)))
(test "self-evaluating number" 145932 145932)
(test "self-evaluating string" "abc" "abc")
(test "self-evaluating boolean" #t #t)
(test "self-evaluating char" #\a #\a)

(test-section "4.1.3 Procedure calls")
(test "simple call" 7 (+ 3 4))
(test "nested call" 12 ((if #f + *) 3 4))

(test-section "4.1.4 Lambda expressions")
(test "simple lambda" 8 ((lambda (x) (+ x x)) 4))
(test "lambda with rest" '(3 4 5 6) ((lambda (x y . z) z) 1 2 3 4 5 6))
(test "lambda rest all" '(1 2 3) ((lambda x x) 1 2 3))

(test-section "4.1.5 Conditionals")
(test "if true" 'yes (if (> 3 2) 'yes 'no))
(test "if false" 'no (if (> 2 3) 'yes 'no))
(test "if no else" 'yes (if (> 3 2) 'yes))

(test-section "4.1.6 Assignments")
(define y 2)
(test "set!" 5 (begin (set! y 5) y))

;;; ============================================================================
;;; 4.2 Derived expression types
;;; ============================================================================

(test-section "4.2.1 Conditionals (cond, case, and, or)")
(test "cond basic" 'greater
      (cond ((> 3 2) 'greater)
            ((< 3 2) 'less)))
(test "cond else" 'equal
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal)))
(test "cond =>" 2
      (cond ((assv 'b '((a 1) (b 2))) => cadr)
            (else #f)))

(test "case symbol" 'composite
      (case (* 2 3)
        ((2 3 5 7) 'prime)
        ((1 4 6 8 9) 'composite)))
(test "case else" 'consonant
      (case (car '(c d))
        ((a e i o u) 'vowel)
        (else 'consonant)))

(test "and empty" #t (and))
(test "and all true" 'foo (and (= 2 2) (> 2 1) 'foo))
(test "and short circuit" #f (and 1 2 #f 'never))
(test "or empty" #f (or))
(test "or first true" 'yes (or 'yes 'no))
(test "or short circuit" 2 (or #f 2 'never))

(test-section "4.2.2 Binding constructs")
(test "let basic" 6 (let ((x 2) (y 3)) (* x y)))
(test "let shadow" 35
      (let ((x 2) (y 3))
        (let ((x 7) (z (+ x y)))
          (* z x))))
(test "let* sequential" 70
      (let ((x 2) (y 3))
        (let* ((x 7) (z (+ x y)))
          (* z x))))
(test "letrec mutual" #t
      (letrec ((even? (lambda (n)
                        (if (zero? n) #t (odd? (- n 1)))))
               (odd? (lambda (n)
                       (if (zero? n) #f (even? (- n 1))))))
        (even? 88)))

(test-section "4.2.3 Sequencing")
(define seq-x 0)
(test "begin" 6 (begin (set! seq-x 5) (+ seq-x 1)))

(test-section "4.2.4 Iteration")
(test "do basic" 10
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum i)))
          ((> i 4) sum)))
(test "do vector" '#(0 1 2 3 4)
      (do ((vec (make-vector 5))
           (i 0 (+ i 1)))
          ((= i 5) vec)
        (vector-set! vec i i)))
(test "named let" '((6 1 3) (-5 -2))
      (let loop ((numbers '(3 -2 1 6 -5))
                 (nonneg '())
                 (neg '()))
        (cond ((null? numbers) (list nonneg neg))
              ((>= (car numbers) 0)
               (loop (cdr numbers)
                     (cons (car numbers) nonneg)
                     neg))
              (else
               (loop (cdr numbers)
                     nonneg
                     (cons (car numbers) neg))))))

(test-section "4.2.5 Delayed evaluation")
(test "delay/force" 3 (force (delay (+ 1 2))))
(test "force memoized" 6
      (let ((p (delay (+ 1 5))))
        (+ (force p) 0)))

(test-section "4.2.6 Quasiquotation")
(test "quasiquote basic" '(list 3 4) `(list ,(+ 1 2) 4))
(test "quasiquote splicing" '(1 2 3 4) `(1 ,@'(2 3) 4))

;;; ============================================================================
;;; 4.3 Macros
;;; ============================================================================

(test-section "4.3 Macros")
(define-syntax my-when
  (syntax-rules ()
    ((my-when test stmt1 stmt2 ...)
     (if test (begin stmt1 stmt2 ...)))))

(define when-result 0)
(test "define-syntax" 42
      (begin
        (my-when #t (set! when-result 42))
        when-result))

(define-syntax my-swap!
  (syntax-rules ()
    ((my-swap! a b)
     (let ((temp a))
       (set! a b)
       (set! b temp)))))

(test "hygiene" '(2 1)
      (let ((a 1) (b 2))
        (my-swap! a b)
        (list a b)))

;;; ============================================================================
;;; 5. Program structure
;;; ============================================================================

(test-section "5.2 Definitions")
(define add3 (lambda (x) (+ x 3)))
(test "define lambda" 6 (add3 3))

(define (square x) (* x x))
(test "define shorthand" 25 (square 5))

(define (sum-squares x y) (+ (square x) (square y)))
(test "internal define" 25 (sum-squares 3 4))

;;; ============================================================================
;;; 6.1 Equivalence predicates
;;; ============================================================================

(test-section "6.1 Equivalence predicates")
(test "eqv? symbols" #t (eqv? 'a 'a))
(test "eqv? numbers" #t (eqv? 2 2))
(test "eqv? chars" #t (eqv? #\a #\a))
(test "eqv? empty lists" #t (eqv? '() '()))
(test "eqv? same pair" #t (let ((p (cons 1 2))) (eqv? p p)))
(test "eqv? diff pairs" #f (eqv? (cons 1 2) (cons 1 2)))

(test "eq? symbols" #t (eq? 'a 'a))
(test "eq? same pair" #t (let ((p '(a))) (eq? p p)))

(test "equal? lists" #t (equal? '(a b c) '(a b c)))
(test "equal? strings" #t (equal? "abc" "abc"))
(test "equal? vectors" #t (equal? '#(a b c) '#(a b c)))

;;; ============================================================================
;;; 6.2 Numbers
;;; ============================================================================

(test-section "6.2 Numbers")
(test "number?" #t (number? 3))
(test "complex?" #t (complex? 3))
(test "real?" #t (real? 3))
(test "rational?" #t (rational? 3))
(test "integer?" #t (integer? 3))

(test "exact?" #t (exact? 3))
(test "inexact?" #t (inexact? 3.14))

(test "= equal" #t (= 1 1 1))
(test "< ascending" #t (< 1 2 3))
(test "> descending" #t (> 3 2 1))
(test "<= non-strict" #t (<= 1 1 2))
(test ">= non-strict" #t (>= 2 2 1))

(test "zero?" #t (zero? 0))
(test "positive?" #t (positive? 1))
(test "negative?" #t (negative? -1))
(test "odd?" #t (odd? 3))
(test "even?" #t (even? 4))

(test "max" 5 (max 1 5 3))
(test "min" 1 (min 5 1 3))
(test "+" 6 (+ 1 2 3))
(test "-" 2 (- 5 3))
(test "- unary" -5 (- 5))
(test "*" 24 (* 2 3 4))
(test "/" 2 (/ 6 3))

(test "abs positive" 7 (abs 7))
(test "abs negative" 7 (abs -7))

(test "quotient" 3 (quotient 10 3))
(test "remainder" 1 (remainder 10 3))
(test "modulo positive" 1 (modulo 13 4))
(test "modulo negative" 3 (modulo -13 4))

(test "gcd" 4 (gcd 32 -36))
(test "lcm" 288 (lcm 32 -36))

(test "floor" -5.0 (floor -4.3))
(test "ceiling" -4.0 (ceiling -4.3))
(test "truncate" -4.0 (truncate -4.3))
(test "round" 4.0 (round 3.5))

(test "exact->inexact" 3.0 (exact->inexact 3))
(test "inexact->exact" 3 (inexact->exact 3.0))

;;; ============================================================================
;;; 6.3 Other data types
;;; ============================================================================

(test-section "6.3.1 Booleans")
(test "not false" #t (not #f))
(test "not true" #f (not #t))
(test "not 3" #f (not 3))
(test "not empty-list" #f (not '()))
(test "boolean? true" #t (boolean? #t))
(test "boolean? false" #t (boolean? #f))
(test "boolean? 0" #f (boolean? 0))

(test-section "6.3.2 Pairs and lists")
(test "pair? cons" #t (pair? '(a . b)))
(test "pair? list" #t (pair? '(a b c)))
(test "pair? empty" #f (pair? '()))
(test "cons" '(a . b) (cons 'a 'b))
(test "car" 'a (car '(a b c)))
(test "cdr" '(b c) (cdr '(a b c)))

(define pair-test (cons 'a 'b))
(set-car! pair-test 'c)
(test "set-car!" 'c (car pair-test))
(set-cdr! pair-test 'd)
(test "set-cdr!" 'd (cdr pair-test))

(test "null? empty" #t (null? '()))
(test "null? list" #f (null? '(1)))
(test "list? proper" #t (list? '(a b c)))
(test "list? improper" #f (list? '(a . b)))
(test "list? empty" #t (list? '()))

(test "list" '(a 7 c) (list 'a (+ 3 4) 'c))
(test "length" 3 (length '(a b c)))
(test "append" '(x y a b) (append '(x y) '(a b)))
(test "reverse" '(c b a) (reverse '(a b c)))
(test "list-ref" 'c (list-ref '(a b c d) 2))

(test "memq found" '(a b c) (memq 'a '(a b c)))
(test "memq not found" #f (memq 'a '(b c d)))
(test "member found" '((a) c) (member '(a) '(b (a) c)))
(test "assq found" '(a 1) (assq 'a '((a 1) (b 2))))
(test "assoc found" '((a)) (assoc '(a) '(((a)) ((b)))))

(test-section "6.3.3 Symbols")
(test "symbol?" #t (symbol? 'foo))
(test "symbol? nil" #t (symbol? 'nil))
(test "symbol? not empty list" #f (symbol? '()))
(test "symbol->string" "flying-fish" (symbol->string 'flying-fish))
;; Note: Reader folds case, so compare via symbol->string
(test "string->symbol" "mISSISSIppi"
      (symbol->string (string->symbol "mISSISSIppi")))

(test-section "6.3.4 Characters")
(test "char?" #t (char? #\a))
(test "char=?" #t (char=? #\a #\a))
(test "char<?" #t (char<? #\a #\b))
(test "char-alphabetic?" #t (char-alphabetic? #\a))
(test "char-numeric?" #t (char-numeric? #\0))
(test "char-whitespace?" #t (char-whitespace? #\space))
(test "char-upcase" #\A (char-upcase #\a))
(test "char-downcase" #\a (char-downcase #\A))
(test "char->integer" 65 (char->integer #\A))
(test "integer->char" #\A (integer->char 65))

(test-section "6.3.5 Strings")
(test "string?" #t (string? "hello"))
(test "make-string" "aaa" (make-string 3 #\a))
(test "string" "abc" (string #\a #\b #\c))
(test "string-length" 5 (string-length "hello"))
(test "string-ref" #\e (string-ref "hello" 1))
(test "string=?" #t (string=? "abc" "abc"))
(test "string<?" #t (string<? "abc" "abd"))
(test "substring" "ell" (substring "hello" 1 4))
(test "string-append" "hello world" (string-append "hello" " " "world"))
(test "string->list" '(#\a #\b) (string->list "ab"))
(test "list->string" "ab" (list->string '(#\a #\b)))
(test "string-copy" "abc" (string-copy "abc"))

(test-section "6.3.6 Vectors")
(test "vector?" #t (vector? '#(1 2 3)))
(test "make-vector" '#(a a a) (make-vector 3 'a))
(test "vector" '#(a b c) (vector 'a 'b 'c))
(test "vector-length" 3 (vector-length '#(a b c)))
(test "vector-ref" 8 (vector-ref '#(1 1 2 3 5 8) 5))
(test "vector->list" '(a b c) (vector->list '#(a b c)))
(test "list->vector" '#(a b c) (list->vector '(a b c)))

;;; ============================================================================
;;; 6.4 Control features
;;; ============================================================================

(test-section "6.4 Control features")
(test "procedure? lambda" #t (procedure? (lambda (x) x)))
(test "procedure? car" #t (procedure? car))
(test "apply" 7 (apply + '(3 4)))
(test "apply with args" 10 (apply + 1 2 '(3 4)))

(test "map single" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "map multiple" '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(test "for-each" 6
      (let ((sum 0))
        (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3))
        sum))
(test "for-each multiple stops at shortest" '(5 7)
      (let ((seen '()))
        (for-each (lambda (x y) (set! seen (cons (+ x y) seen)))
                  '(1 2 3)
                  '(4 5))
        (reverse seen)))

(test "call/cc escape" 'return-value
      (call-with-current-continuation
        (lambda (exit)
          (for-each (lambda (x)
                      (if (negative? x)
                          (exit 'return-value)))
                    '(1 2 -3 4))
          'normal)))

(test "call/cc values" 10
      (+ 1 (call/cc (lambda (k) (+ 2 (k 3) 4))) 6))

(test "values/call-with-values" 5
      (call-with-values (lambda () (values 2 3)) +))

;;; ============================================================================
;;; 6.5 Eval
;;; ============================================================================

(test-section "6.5 Eval")
(test "eval basic" 21 (eval '(* 7 3) (scheme-report-environment 5)))
;; null-environment only has syntax, not procedures
(test "eval lambda" 42 (eval '((lambda (x) x) 42) (null-environment 5)))

;;; ============================================================================
;;; 6.6 Input and output
;;; ============================================================================

(test-section "6.6 Input and output")
(test "input-port?" #t (input-port? (current-input-port)))
(test "output-port?" #t (output-port? (current-output-port)))

;; String port tests
(test "open-output-string" "hello"
      (let ((p (open-output-string)))
        (display "hello" p)
        (get-output-string p)))

(test "open-input-string" #\h
      (let ((p (open-input-string "hello")))
        (read-char p)))

;;; ============================================================================
;;; Additional R5RS Tests
;;; ============================================================================

(test-section "Lambda variants")
(test "lambda double" 8 ((lambda (x) (+ x x)) 4))
(test "lambda rest all" '(3 4 5 6) ((lambda x x) 3 4 5 6))
(test "lambda rest partial" '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(test-section "Conditionals extended")
(test "if true symbol" 'yes (if (> 3 2) 'yes 'no))
(test "if false symbol" 'no (if (> 2 3) 'yes 'no))
(test "if with computation" 1 (if (> 3 2) (- 3 2) (+ 3 2)))
(test "cond two clauses" 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
(test "cond else" 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))
(test "case composite" 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
(test "case evaluates key once" 1
    (let ((count 0))
      (case (begin (set! count (+ count 1)) 'c)
        ((a) 'a)
        ((b) 'b)
        ((c) count)
        (else 'other))))
(test "case else" 'consonant
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else 'consonant)))

(test-section "And/Or extended")
(test "and true" #t (and (= 2 2) (> 2 1)))
(test "and false" #f (and (= 2 2) (< 2 1)))
(test "and last value" '(f g) (and 1 2 'c '(f g)))
(test "and empty" #t (and))
(test "or both true" #t (or (= 2 2) (> 2 1)))
(test "or first true" #t (or (= 2 2) (< 2 1)))
(test "or short circuit" '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test-section "Let variants extended")
(test "let product" 6 (let ((x 2) (y 3)) (* x y)))
(test "let nested shadow" 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
(test "let* sequential ref" 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test "let internal define" -2 (let ()
           (define x 2)
           (define f (lambda () (- x)))
           (f)))

;; Test that internal defines don't leak
(define let*-def 1)
(let* () (define let*-def 2) #f)
(test "let* def doesn't leak" 1 let*-def)

(test-section "Do extended")
(test "do build vector" '#(0 1 2 3 4)
 (do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
   (vector-set! vec i i)))

(test "do sum list" 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x) sum))))

(test "named let partition" '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
      (cond
       ((null? numbers) (list nonneg neg))
       ((>= (car numbers) 0)
        (loop (cdr numbers) (cons (car numbers) nonneg) neg))
       ((< (car numbers) 0)
        (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

(test-section "Quasiquote extended")
(test "quasiquote list" '(list 3 4) `(list ,(+ 1 2) 4))
(test "quasiquote quote" '(list a 'a) (let ((name 'a)) `(list ,name ',name)))
(test "quasiquote splice" '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
;; 4^4 = 256, 3^3 = 27
(test "quasiquote expt" '(10 5 4 256 27 8)
    `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n n)) '(4 3)) 8))
(test "quasiquote nested" '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(test "quasiquote nested unquote" '(a `(b ,x ,'y d) e)
    (let ((name1 'x) (name2 'y))
      `(a `(b ,,name1 ,',name2 d) e)))
(test "quasiquote explicit" '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)))

(test-section "Equivalence extended")
(test "eqv? symbols same" #t (eqv? 'a 'a))
(test "eqv? symbols diff" #f (eqv? 'a 'b))
(test "eqv? empty lists" #t (eqv? '() '()))
(test "eqv? new cons" #f (eqv? (cons 1 2) (cons 1 2)))
(test "eqv? diff lambdas" #f (eqv? (lambda () 1) (lambda () 2)))
(test "eqv? same lambda" #t (let ((p (lambda (x) x))) (eqv? p p)))
(test "eq? same symbol" #t (eq? 'a 'a))
(test "eq? new lists" #f (eq? (list 'a) (list 'a)))
(test "eq? empty" #t (eq? '() '()))
(test "eq? car" #t (eq? car car))
(test "eq? same obj" #t (let ((x '(a))) (eq? x x)))
(test "eq? same proc" #t (let ((p (lambda (x) x))) (eq? p p)))
(test "equal? symbol" #t (equal? 'a 'a))
(test "equal? one elem list" #t (equal? '(a) '(a)))
(test "equal? nested list" #t (equal? '(a (b) c) '(a (b) c)))
(test "equal? string same" #t (equal? "abc" "abc"))
(test "equal? string diff len" #f (equal? "abc" "abcd"))
(test "equal? string diff" #f (equal? "a" "b"))
(test "equal? numbers" #t (equal? 2 2))
(test "equal? vectors" #t (equal? (make-vector 5 'a) (make-vector 5 'a)))

(test-section "Arithmetic extended")
(test "max simple" 4 (max 3 4))
(test "+ three" 7 (+ 3 4))
(test "+ one" 3 (+ 3))
(test "+ zero" 0 (+))
(test "* one" 4 (* 4))
(test "* zero" 1 (*))
(test "- two" -1 (- 3 4))
(test "- three" -6 (- 3 4 5))
(test "- one" -3 (- 3))
(test "- mixed" -1.0 (- 3.0 4))
(test "abs neg" 7 (abs -7))
(test "mod pos pos" 1 (modulo 13 4))
(test "rem pos pos" 1 (remainder 13 4))
(test "mod neg pos" 3 (modulo -13 4))
(test "rem neg pos" -1 (remainder -13 4))
(test "mod pos neg" -3 (modulo 13 -4))
(test "rem pos neg" 1 (remainder 13 -4))
(test "mod neg neg" -1 (modulo -13 -4))
(test "rem neg neg" -1 (remainder -13 -4))
(test "gcd" 4 (gcd 32 -36))
(test "lcm" 288 (lcm 32 -36))

(test-section "Number string conversion extended")
(test "str->num 100" 100 (string->number "100"))
(test "str->num hex" 256 (string->number "100" 16))
(test "str->num oct" 127 (string->number "177" 8))
(test "str->num bin" 5 (string->number "101" 2))
(test "str->num exp" 100.0 (string->number "1e2"))
(test "num->str 100" "100" (number->string 100))
(test "num->str hex 256" "100" (number->string 256 16))
(test "num->str hex 255" "ff" (number->string 255 16))
(test "num->str oct" "177" (number->string 127 8))
(test "num->str bin" "101" (number->string 5 2))

(test-section "Boolean extended")
(test "not 3" #f (not 3))
(test "not list" #f (not (list 3)))
(test "boolean? 0" #f (boolean? 0))

(test-section "Pairs and lists extended")
(test "pair? dotted" #t (pair? '(a . b)))
(test "pair? list" #t (pair? '(a b c)))
(test "cons a nil" '(a) (cons 'a '()))
(test "cons list list" '((a) b c d) (cons '(a) '(b c d)))
(test "cons str list" '("a" b c) (cons "a" '(b c)))
(test "cons a 3" '(a . 3) (cons 'a 3))
(test "cons list sym" '((a b) . c) (cons '(a b) 'c))
(test "car simple" 'a (car '(a b c)))
(test "car nested" '(a) (car '((a) b c d)))
(test "car dotted" 1 (car '(1 . 2)))
(test "cdr nested" '(b c d) (cdr '((a) b c d)))
(test "cdr dotted" 2 (cdr '(1 . 2)))
(test "list? proper" #t (list? '(a b c)))
(test "list? empty" #t (list? '()))
(test "list? improper" #f (list? '(a . b)))
(test "list? circular" #f
    (let ((x (list 'a)))
      (set-cdr! x x)
      (list? x)))
(test "list mixed" '(a 7 c) (list 'a (+ 3 4) 'c))
(test "list empty" '() (list))
(test "length simple" 3 (length '(a b c)))
(test "length nested" 3 (length '(a (b) (c d e))))
(test "length empty" 0 (length '()))
(test "append x y" '(x y) (append '(x) '(y)))
(test "append a bcd" '(a b c d) (append '(a) '(b c d)))
(test "append nested" '(a (b) (c)) (append '(a (b)) '((c))))
(test "append improper" '(a b c . d) (append '(a b) '(c . d)))
(test "append empty sym" 'a (append '() 'a))
(test "reverse simple" '(c b a) (reverse '(a b c)))
(test "reverse nested" '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))
(test "list-ref" 'c (list-ref '(a b c d) 2))
(test "memq found first" '(a b c) (memq 'a '(a b c)))
(test "memq found mid" '(b c) (memq 'b '(a b c)))
(test "memq not found" #f (memq 'a '(b c d)))
(test "memq no equal?" #f (memq (list 'a) '(b (a) c)))
(test "member equal?" '((a) c) (member (list 'a) '(b (a) c)))
(test "memv" '(101 102) (memv 101 '(100 101 102)))
(test "assq not found" #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(test "assoc found" '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(test "assv" '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test-section "Symbols extended")
(test "symbol? sym" #t (symbol? 'foo))
(test "symbol? car sym" #t (symbol? (car '(a b))))
(test "symbol? string" #f (symbol? "bar"))
(test "symbol->string" "flying-fish" (symbol->string 'flying-fish))
(test "string->symbol roundtrip" "Malvina" (symbol->string (string->symbol "Malvina")))

(test-section "Strings extended")
(test "string? str" #t (string? "a"))
(test "string? sym" #f (string? 'a))
(test "string-length empty" 0 (string-length ""))
(test "string-length abc" 3 (string-length "abc"))
(test "string-ref 0" #\a (string-ref "abc" 0))
(test "string-ref 2" #\c (string-ref "abc" 2))
(test "string=? same char" #t (string=? "a" (string #\a)))
(test "string=? diff char" #f (string=? "a" (string #\b)))
(test "string<? shorter" #t (string<? "a" "aa"))
(test "string<? longer" #f (string<? "aa" "a"))
(test "string<? equal" #f (string<? "a" "a"))
(test "string<=? shorter" #t (string<=? "a" "aa"))
(test "string<=? equal" #t (string<=? "a" "a"))
(test "make-string match" #t (string=? "a" (make-string 1 #\a)))
(test "make-string diff" #f (string=? "a" (make-string 1 #\b)))
(test "substring empty" "" (substring "abc" 0 0))
(test "substring one" "a" (substring "abc" 0 1))
(test "substring end" "bc" (substring "abc" 1 3))
(test "string-append right empty" "abc" (string-append "abc" ""))
(test "string-append left empty" "abc" (string-append "" "abc"))
(test "string-append both" "abc" (string-append "a" "bc"))

(test-section "Vectors extended")
(test "vector-set!" '#(0 ("Sue" "Sue") "Anna")
 (let ((vec (vector 0 '(2 2 2 2) "Anna")))
   (vector-set! vec 1 '("Sue" "Sue"))
   vec))
(test "vector->list" '(dah dah didah) (vector->list '#(dah dah didah)))
(test "list->vector" '#(dididit dah) (list->vector '(dididit dah)))

(test-section "Procedures extended")
(test "procedure? car" #t (procedure? car))
(test "procedure? symbol" #f (procedure? 'car))
(test "procedure? lambda" #t (procedure? (lambda (x) (* x x))))
(test "procedure? quoted lambda" #f (procedure? '(lambda (x) (* x x))))
(test "procedure? continuation" #t (call-with-current-continuation procedure?))

(test-section "Control extended")
(test "call/cc simple" 7 (call-with-current-continuation (lambda (k) (+ 2 5))))
(test "call/cc escape" 3 (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))
(test "apply list" 7 (apply + (list 3 4)))
(test "map cadr" '(b e h) (map cadr '((a b) (d e) (g h))))
(test "map expt" '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))
(test "map multi" '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(test "for-each vector" '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4))
      v))
(test "force simple" 3 (force (delay (+ 1 2))))
(test "force memoized" '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))
;; force used to recurse through a delay-force chain (one C/interpreter
;; stack frame per link), so a long chain of delay-forced promises - the
;; standard lazy-stream idiom - could overflow the stack. force is now
;; iterative, so this must complete in constant stack space per R7RS 4.2.5.
(define (%delay-force-chain n)
  (delay-force (if (= n 0) (delay 'done) (%delay-force-chain (- n 1)))))
(test "delay-force chain runs in constant stack space" 'done
    (force (%delay-force-chain 100000)))

(test-section "Hygiene edge cases")
(test "else as binding" 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))
(test "=> as binding" 'ok (let ((=> 1)) (cond (#t => 'ok))))
(test "unquote as binding" '((unquote foo)) (let ((unquote 1)) `(,foo)))
(test "unquote-splicing as binding" '((unquote-splicing foo)) (let ((unquote-splicing 1)) `(,@foo)))
(test "... as binding" 'ok
    (let ((... 2))
      (let-syntax ((s (syntax-rules ()
                        ((_ x ...) 'bad)
                        ((_ . r) 'ok))))
        (s a b c))))
(test "let-syntax internal def" 'ok (let ()
            (let-syntax ()
              (define internal-def 'ok))
            internal-def))
(test "letrec-syntax internal def" 'ok (let ()
            (letrec-syntax ()
              (define internal-def 'ok))
            internal-def))

(test-section "Set! variants")
(test "set! in let" '(2 1)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (list x y))))))
(test "set! before let" '(2 2)
    ((lambda () (let ((x 1)) (set! x 2) (let ((y x)) (list x y))))))
(test "set! inner only" '(1 2)
    ((lambda () (let ((x 1)) (let ((y x)) (set! y 2) (list x y))))))
(test "set! both" '(2 3)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (set! y 3) (list x y))))))

(test-section "Dynamic-wind extended")
(test "dynamic-wind order" '(a b c)
    (let* ((path '())
           (add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))
      (reverse path)))
(test "dynamic-wind preserves direct multiple values" '(1 2)
    (call-with-values
      (lambda ()
        (dynamic-wind (lambda () #f)
                      (lambda () (values 1 2))
                      (lambda () #f)))
      list))

(test "dynamic-wind with continuation" '(connect talk1 disconnect connect talk2 disconnect)
    (let ((path '())
          (c #f))
      (let ((add (lambda (s) (set! path (cons s path)))))
        (dynamic-wind
            (lambda () (add 'connect))
            (lambda ()
              (add (call-with-current-continuation
                    (lambda (c0)
                      (set! c c0)
                      'talk1))))
            (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path)))))

(test "dynamic-wind ancestor jump leaves shared outer wind untouched"
    '(start before-a before-b body after-b before-b body after-b after-a done)
    (let ((log '()) (k1 #f) (invoked #f))
      (define (add x) (set! log (cons x log)))
      (add 'start)
      (dynamic-wind
        (lambda () (add 'before-a))
        (lambda ()
          (call/cc (lambda (k) (set! k1 k)))
          (dynamic-wind
            (lambda () (add 'before-b))
            (lambda ()
              (add 'body)
              (if (not invoked) (begin (set! invoked #t) (k1 #f))))
            (lambda () (add 'after-b))))
        (lambda () (add 'after-a)))
      (add 'done)
      (reverse log)))

(test "dynamic-wind descendant jump re-enters only the inner wind"
    '(start before-a before-b body-b after-b mid before-b after-b mid after-a done)
    (let ((log '()) (k1 #f) (invoked #f))
      (define (add x) (set! log (cons x log)))
      (add 'start)
      (dynamic-wind
        (lambda () (add 'before-a))
        (lambda ()
          (dynamic-wind
            (lambda () (add 'before-b))
            (lambda ()
              (add 'body-b)
              (call/cc (lambda (k) (set! k1 k))))
            (lambda () (add 'after-b)))
          (add 'mid)
          (if (not invoked) (begin (set! invoked #t) (k1 #f))))
        (lambda () (add 'after-a)))
      (add 'done)
      (reverse log)))

(test "dynamic-wind unwinds across an unequal-depth sibling jump"
    '(before-a before-b before-c body-c after-c before-c body-c after-c after-b after-a)
    (let ((log '()) (kc #f) (invoked #f))
      (define (add x) (set! log (cons x log)))
      (dynamic-wind
        (lambda () (add 'before-a))
        (lambda ()
          (dynamic-wind
            (lambda () (add 'before-b))
            (lambda ()
              (call/cc (lambda (k) (set! kc k)))
              (dynamic-wind
                (lambda () (add 'before-c))
                (lambda ()
                  (add 'body-c)
                  (if (not invoked) (begin (set! invoked #t) (kc #f))))
                (lambda () (add 'after-c))))
            (lambda () (add 'after-b))))
        (lambda () (add 'after-a)))
      (reverse log)))

(test-section "Syntax-rules ellipsis renaming")
(test "custom ellipsis" 2 (let-syntax
            ((foo (syntax-rules ::: ()
                    ((foo ... args :::)
                     (args ::: ...)))))
          (foo 3 - 5)))

(test "ellipsis at end" '(5 4 1 2 3)
    (let-syntax
        ((foo (syntax-rules ()
                ((foo args ... penultimate ultimate)
                 (list ultimate penultimate args ...)))))
      (foo 1 2 3 4 5)))

(test-section "Ellipsis semantics")
(test "empty list under ellipsis" '(() 1 2)
    (let-syntax ((foo (syntax-rules () ((_ x ...) '(x ...)))))
      (foo () 1 2)))
(test "parallel vars with empty list" '((1 2) (() 3))
    (let-syntax ((foo (syntax-rules () ((_ (a b) ...) '((a ...) (b ...))))))
      (foo (1 ()) (2 3))))
(test "nested ellipsis with empty group" '((1) () (2))
    (let-syntax ((foo (syntax-rules () ((_ (x ...) ...) '((x ...) ...)))))
      (foo (1) () (2))))
(test "greedy ellipsis with dotted tail" '((1 2 3) ())
    (let-syntax ((foo (syntax-rules () ((_ x ... . r) '((x ...) r)))))
      (foo 1 2 3)))
(test "ellipsis escape" '(... ...)
    (let-syntax ((foo (syntax-rules () ((_) '((... ...) (... ...))))))
      (foo)))
(test "macro-writing macro" '(1 2 3)
    (let ()
      (define-syntax gen
        (syntax-rules ()
          ((_ name)
           (define-syntax name
             (syntax-rules () ((_ x (... ...)) '(x (... ...))))))))
      (gen collect)
      (collect 1 2 3)))
(test "triple-nested ellipsis" '(((1 2) (3)) ((4 5)))
    (let-syntax ((f (syntax-rules ()
                      ((_ ((x ...) ...) ...) '(((x ...) ...) ...)))))
      (f ((1 2) (3)) ((4 5)))))
(test "triple-nested with empty groups" '((() (6)) () ((7) (8 9)))
    (let-syntax ((f (syntax-rules ()
                      ((_ ((x ...) ...) ...) '(((x ...) ...) ...)))))
      (f (() (6)) () ((7) (8 9)))))
(test "nested vector ellipsis" '((1 2) (3 4) (5 6))
    (let-syntax ((f (syntax-rules ()
                      ((_ #(#(x ...) ...)) '((x ...) ...)))))
      (f #(#(1 2) #(3 4) #(5 6)))))
(test "vector ellipsis with pre and post" '(1 (2 3) 4)
    (let-syntax ((f (syntax-rules ()
                      ((_ #(a x ... b)) '(a (x ...) b)))))
      (f #(1 2 3 4))))
;; A self-evaluating literal in a pattern (string, bignum, ...) used to be
;; matched by comparing raw cell identity instead of content, so a literal
;; that happened to be a separately-allocated (but equal?) cell - the
;; normal case, since the pattern's literal and the use site's argument
;; are read from different source text - could never match.
(test "string literal pattern matches by content" 'matched
    (let-syntax ((f (syntax-rules ()
                      ((_ "hello") 'matched)
                      ((_ s) (list 'nomatch s)))))
      (f "hello")))
(test "string literal pattern still falls through on mismatch" '(nomatch "world")
    (let-syntax ((f (syntax-rules ()
                      ((_ "hello") 'matched)
                      ((_ s) (list 'nomatch s)))))
      (f "world")))
(test "bignum literal pattern matches by content" 'matched-big
    (let-syntax ((f (syntax-rules ()
                      ((_ 100000000000000000000) 'matched-big)
                      ((_ s) (list 'nomatch s)))))
      (f 100000000000000000000)))

(test-section "Quasiquote semantics")
(test "dotted tail unquote" '(1 . 3) `(1 . ,(+ 1 2)))
(test "splice then dotted tail" '(1 2 3 . 4) `(1 ,@(list 2 3) . 4))
(test "nested quasiquote stays literal" '(a (quasiquote (b (unquote (c)))))
    `(a `(b ,(c))))
(test "double unquote evaluates" '(quasiquote (a (unquote 3)))
    ``(a ,,(+ 1 2)))
(test "depth-2 splicing stays literal" '(quasiquote ((unquote-splicing x)))
    ``(,@x))
(test "vector unquote" '#(1 2 3) `#(1 ,(+ 1 1) 3))
(test "continuation re-enters quasiquote" '(a 99)
    (let ((k #f) (first #t))
      (let ((result `(a ,(call-with-current-continuation
                          (lambda (c) (set! k c) 1)))))
        (if first
            (begin (set! first #f) (k 99))
            result))))

(test-section "Exception handling")
(test "guard re-raises to enclosing handler without looping" '(caught-at-top boom)
    (call/cc (lambda (top)
      (with-exception-handler
        (lambda (e) (top (list 'caught-at-top e)))
        (lambda ()
          (guard (e (#f 'never-matches))
            (raise 'boom)))))))
(test "nested guard propagates unmatched condition to outer guard" '(outer boom)
    (guard (e1 (#t (list 'outer e1)))
      (guard (e2 (#f 'never))
        (raise 'boom))))
(test "guard with matching clause catches normally" '(caught x)
    (guard (e (#t (list 'caught e))) (raise 'x)))
(test "guard catches malformed special forms" 'caught
    (guard (e (#t 'caught)) (if)))
(test "guard catches syntax transformer errors" 'caught
    (guard (e (#t 'caught))
      (let-syntax ((m (syntax-rules () ((_ x) x))))
        (m))))
(test "guard catches invalid call/cc arity" 'caught
    (guard (e (#t 'caught)) (call/cc)))
(test "guard catches invalid apply arity" 'caught
    (guard (e (#t 'caught)) (apply)))
(test "raise-continuable returns handler value to raise point" 16
    (with-exception-handler
      (lambda (e) (+ e 1))
      (lambda () (+ 10 (raise-continuable 5)))))
(test "error object carries message through guard" "boom"
    (guard (e (#t (error-object-message e))) (error "boom" 1 2)))
(test "dynamic-wind runs after thunk when body raises" '(caught (before after))
    (let ((events '()))
      (list
       (guard (e (#t 'caught))
         (dynamic-wind
           (lambda () (set! events (cons 'before events)))
           (lambda () (error "boom"))
           (lambda () (set! events (cons 'after events)))))
       (reverse events))))
(test "deep re-raise chain terminates at the matching guard" '(caught-deep deep)
    (letrec ((nest (lambda (n)
                     (if (= n 0)
                         (raise 'deep)
                         (guard (e (#f 'never)) (nest (- n 1)))))))
      (guard (e (#t (list 'caught-deep e))) (nest 10))))

(test-section "Hygiene: definition-site references")
;; These reference genuine TOP-LEVEL globals (each define below is its own
;; already-executed top-level form by the time the macro is compiled) -
;; the scenario the audit reported and this fix targets.
(define (hygiene-test-f) 100)
(define-syntax hygiene-test-callf (syntax-rules () ((_) (hygiene-test-f))))
(test "macro references runtime-defined global" 100
    (let ((hygiene-test-f (lambda () 200))) (hygiene-test-callf)))

(define hygiene-test-x 1)
(define-syntax hygiene-test-getx (syntax-rules () ((_) hygiene-test-x)))
(set! hygiene-test-x 2)
(test "macro sees mutation, not use-site shadow" 2
    (let ((hygiene-test-x 99)) (hygiene-test-getx)))

(define (hygiene-test-g) 'old)
(define-syntax hygiene-test-callg (syntax-rules () ((_) (hygiene-test-g))))
(define (hygiene-test-g) 'new)
(test "macro sees redefinition, not use-site shadow" 'new
    (let ((hygiene-test-g (lambda () 'local))) (hygiene-test-callg)))

(define-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(test "swap! hygiene unaffected by fix" '(2 1 77)
    (let ()
      (define p 1) (define q 2)
      (let ((tmp 77)) (swap! p q) (list p q tmp))))

(define hygiene-test-lst '(1 2 3))
(define-syntax hygiene-test-mylen (syntax-rules () ((_ v) (length v))))
(test "macro sees use-site shadow of a primitive" 3
    (let ((length car)) (hygiene-test-mylen hygiene-test-lst)))

;; define-syntax now resolves free identifiers eagerly at its own
;; definition point (mirroring let-syntax), instead of lazily at each use
;; site via apply_syntax. This closes the internal-define gap that used to
;; make this diverge between VM and interpreter modes.
(test "macro references an internal (non-global) define" 100
    (let ()
      (define (f) 100)
      (define-syntax callf (syntax-rules () ((_) (f))))
      (let ((f (lambda () 200))) (callf))))
(test "internal define hygiene holds across repeated invocations" '(100 100)
    (letrec ((make-thing
               (lambda ()
                 (define (f) 100)
                 (define-syntax callf (syntax-rules () ((_) (f))))
                 (let ((f (lambda () 200))) (callf)))))
      (list (make-thing) (make-thing))))
(test "internal define-syntax referencing mutable local state" '(3 3)
    (letrec ((counter-maker
               (lambda ()
                 (define n 0)
                 (define-syntax bump! (syntax-rules () ((_) (set! n (+ n 1)))))
                 (bump!) (bump!) (bump!)
                 n)))
      (list (counter-maker) (counter-maker))))

(test-section "Hygiene: no-op free-identifier renames are skipped")
;; apply_syntax used to mint a fresh gensym for EVERY free identifier on
;; EVERY expansion, then alias it into the use-site frame. When the
;; identifier already denotes the same binding cell at the use site as at
;; the definition site, that rename is a no-op, so it is now skipped - one
;; permanent atom-table slot per expansion was the cost, which exhausted
;; the table in --interpreter mode (the CPS evaluator re-expands a macro on
;; every evaluation, unlike the VM which expands once at compile time).
;; These pin the boundaries of that skip: it must NOT fire whenever the
;; identifier genuinely resolves somewhere else at the use site.
(define noop-rename-x 5)
(define-syntax noop-rename-inc (syntax-rules () ((_ v) (+ v 1))))

;; Skip fires here: `+` is the same global at both sites.
(test "unshadowed free identifier still resolves" 6
    (noop-rename-inc noop-rename-x))
(test "skip path stays correct across repeated expansions" '(6 6 6)
    (list (noop-rename-inc 5) (noop-rename-inc 5) (noop-rename-inc 5)))

;; Skip must NOT fire below - the definition-site binding must win.
(test "free identifier shadowed by a let at the use site" 11
    (let ((+ -)) (noop-rename-inc 10)))
(test "free identifier shadowed by a lambda parameter" 11
    ((lambda (+) (noop-rename-inc 10)) -))
(test "free identifier shadowed by an internal define, repeated calls" '(11 21)
    (letrec ((shadowing
               (lambda (n)
                 (define + -)
                 (noop-rename-inc n))))
      (list (shadowing 10) (shadowing 20))))

;; A transformer-introduced identifier that is bound only at the use site
;; must still be renamed to an unbound gensym, so the use-site binding
;; cannot capture it.
(define-syntax noop-rename-usey (syntax-rules () ((_) noop-rename-free-y)))
(test "use-site binding cannot capture a macro-introduced identifier" 'unbound
    (let ((noop-rename-free-y 99))
      (guard (e (#t 'unbound)) (noop-rename-usey))))

(test-section "Macro expansion timing")
(define-syntax redefine-hygiene-m (syntax-rules () ((_ x) (+ x 1))))
(define (redefine-hygiene-f) (redefine-hygiene-m 5))
(define-syntax redefine-hygiene-m (syntax-rules () ((_ x) (* x 10))))
(test "macro use in previously defined procedure uses definition-time macro" 6
    (redefine-hygiene-f))

(test-section "Named-let hygiene under allocation pressure")
;; hygienize_named_let held new_sym (the gensym for a renamed named-let
;; variable) as a raw local across alloc_cons/list_append calls in its binding
;; loop, reachable only through the rename list - so a GC mid-loop could
;; relocate the cell and splice a stale index into the rebuilt bindings.
;; hygienize_let and hygienize_lambda both protect the same value. The
;; make-vector below forces collections while the macro is expanded
;; repeatedly. Verified against MIT.
(define-syntax named-let-churn
  (syntax-rules ()
    ((_ a b) (let loop ((x a) (y b) (z 1) (w 2) (v 3))
               (if (= x 0) (+ y z w v) (loop (- x 1) y z w v))))))
(test "named-let expansion survives GC pressure" 7
    (let churn ((n 20000) (acc 7))
      (if (= n 0)
          acc
          (churn (- n 1)
                 (begin (make-vector 40 n)
                        (modulo (named-let-churn 3 acc) 1000))))))

(define named-let-outer 99)
(define-syntax named-let-shadow
  (syntax-rules ()
    ((_ a) (let loop ((named-let-outer a) (k 0))
             (if (= named-let-outer 0) k (loop (- named-let-outer 1) (+ k 1)))))))
(test "named-let variable does not capture a use-site binding" '(5 99)
    (list (named-let-shadow 5) named-let-outer))

(test-section "letrec with 3+ bindings")
;; The CPS interpreter's letrec-init continuation used to silently swap
;; bindings whenever 3+ were in scope (a saved-value list built most-
;; recent-first was replayed as if it were oldest-first). The bytecode VM
;; was never affected; these check the interpreter matches it.
(test "three sequential bindings stay in order" '(1 2 3)
    (letrec* ((a 1) (b 2) (d (+ a b))) (list a b d)))
(test "four sequential bindings stay in order" '(1 2 3 6)
    (letrec* ((a 1) (b 2) (c 3) (d (+ a b c))) (list a b c d)))
(test "ten sequential bindings stay in order" '(0 1 2 3 4 5 6 7 8 9 45)
    (letrec* ((v0 0) (v1 1) (v2 2) (v3 3) (v4 4) (v5 5) (v6 6) (v7 7)
              (v8 8) (v9 9) (s (+ v0 v1 v2 v3 v4 v5 v6 v7 v8 v9)))
      (list v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 s)))
(test "mutual recursion across 3 letrec-bound procedures" '(f-done g-done h-done)
    (letrec ((f (lambda (n) (if (= n 0) 'f-done (g (- n 1)))))
             (g (lambda (n) (if (= n 0) 'g-done (h (- n 1)))))
             (h (lambda (n) (if (= n 0) 'h-done (f (- n 1))))))
      (list (f 0) (g 0) (h 0))))

(test-section "call-with-values continuations")
(test "continuation escapes and resumes call-with-values" 5
    (let ((k #f))
      (let ((v (call-with-values
                 (lambda () (call/cc (lambda (c) (set! k c) 1)))
                 (lambda (x) x))))
        (if (< v 5) (k (+ v 1)) v))))
(test "call-with-values basic" 6
    (call-with-values (lambda () (values 1 2 3)) +))
(test "call-with-values zero values" '()
    (call-with-values (lambda () (values)) list))
(test "call-with-values single value" '(42)
    (call-with-values (lambda () 42) list))
(test "nested call-with-values" '(1 2 3 4)
    (call-with-values
      (lambda ()
        (call-with-values (lambda () (values 1 2)) values))
      (lambda (a b)
        (call-with-values
          (lambda () (call-with-values (lambda () (values 3 4)) values))
          (lambda (c d) (list a b c d))))))
(test "call-with-values producer does real work" 1000000
    (call-with-values
      (lambda ()
        (define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1))))
        (values (loop 1000000 0)))
      (lambda (x) x)))
(test "dynamic-wind around call-with-values" '(in 6 out)
    (let ((log '()))
      (define (add x) (set! log (cons x log)))
      (dynamic-wind
        (lambda () (add 'in))
        (lambda () (add (call-with-values (lambda () (values 1 2 3)) +)))
        (lambda () (add 'out)))
      (reverse log)))
(test "call-with-values consumer is a builtin" 3
    (call-with-values (lambda () (values 1 2)) +))
(test "call-with-values keep-alive across continuation" '((keep-me . 42) 3)
    (let ()
      (define (churn n acc) (if (= n 0) acc (churn (- n 1) (cons n '()))))
      (list (cons 'keep-me 42)
            (call-with-values
              (lambda () (churn 500000 '()) (values 1 2))
              +))))

(test-section "let-values/let*-values/define-values formals")
;; A multi-value <formals> is the same grammar as a lambda parameter list:
;; (var ...), a bare identifier collecting all values as a list, or a
;; dotted (var ... . rest). These used to only accept (var ...).
(test "let-values proper list formals" 6
    (let-values (((a b c) (values 1 2 3))) (+ a b c)))
(test "let-values bare identifier formals" '(1 2 3)
    (let-values ((all (values 1 2 3))) all))
(test "let-values dotted formals" '(1 2 (3 4))
    (let-values (((a b . rest) (values 1 2 3 4))) (list a b rest)))
(test "let*-values bare identifier formals" '((1 2) 1)
    (let*-values ((all (values 1 2)) ((x) (values (car all)))) (list all x)))
(test "let*-values dotted formals" '(1 (2 3))
    (let*-values (((a . rest) (values 1 2 3))) (list a rest)))
(test "define-values proper list formals" '(1 2)
    (let ()
      (define-values (a b) (values 1 2))
      (list a b)))
(test "define-values bare identifier formals" '(5 6 7)
    (let ()
      (define-values all (values 5 6 7))
      all))
(test "define-values dotted formals" '(1 2 (3 4))
    (let ()
      (define-values (a b . rest) (values 1 2 3 4))
      (list a b rest)))

(test-section "parameterize converter semantics")
;; parameterize must apply the converter when installing a new value, but
;; restore the dynamic extent's original value as-is on exit - that value
;; is already-converted, so reapplying the converter would apply it twice
;; for any converter that isn't idempotent (R7RS 4.2.6).
(test "parameterize restores without reconverting" 11
    (let ((p (make-parameter 10 (lambda (x) (+ x 1)))))
      (parameterize ((p 20)) (p))
      (p)))
(test "parameterize converts the new value while active" 21
    (let ((p (make-parameter 10 (lambda (x) (+ x 1)))))
      (parameterize ((p 20)) (p))))
(test "parameterize nesting restores each level without reconverting" '(31 21 11)
    (let ((p (make-parameter 10 (lambda (x) (+ x 1)))) (log '()))
      (parameterize ((p 20))
        (parameterize ((p 30))
          (set! log (cons (p) log)))
        (set! log (cons (p) log)))
      (set! log (cons (p) log))
      (reverse log)))

(test-section "Bytevector ports: peek-u8/u8-ready?")
;; Byte-oriented operations require binary ports.  The wrappers support the
;; vector-based bytevector input ports, while textual ports are rejected.
(test "peek-u8 on bytevector port does not consume" '(1 1)
    (let ((p (open-input-bytevector (bytevector 1 2 3))))
      (list (peek-u8 p) (peek-u8 p))))
(test "peek-u8 then read-u8 on bytevector port" '(1 1 2)
    (let ((p (open-input-bytevector (bytevector 1 2 3))))
      (list (peek-u8 p) (read-u8 p) (read-u8 p))))
(test "peek-u8 at end of bytevector port is eof" #t
    (let ((p (open-input-bytevector (bytevector 1))))
      (read-u8 p)
      (eof-object? (peek-u8 p))))
(test "u8-ready? on open bytevector port" #t
    (u8-ready? (open-input-bytevector (bytevector 1 2 3))))
(test "u8-ready? on exhausted bytevector port" #t
    (let ((p (open-input-bytevector (bytevector 1))))
      (read-u8 p)
      (u8-ready? p)))
(test "peek-u8/u8-ready? reject textual ports" '(#t #t)
    (let ((p (open-input-string "abc")))
      (list (guard (e (#t #t)) (peek-u8 p) #f)
            (guard (e (#t #t)) (u8-ready? p) #f))))

(test-section "string-upper-case?/string-lower-case? with no letters")
(test "string-upper-case? on digits" #t (string-upper-case? "123"))
(test "string-upper-case? on empty string" #t (string-upper-case? ""))
(test "string-upper-case? on punctuation" #t (string-upper-case? "!!!"))
(test "string-upper-case? still false on mixed case" #f (string-upper-case? "ABc"))
(test "string-lower-case? on digits" #t (string-lower-case? "123"))
(test "string-lower-case? on empty string" #t (string-lower-case? ""))
(test "string-lower-case? still false on mixed case" #f (string-lower-case? "abC"))

(test-section "SRFI-1/R7RS-large stdlib procedures")
(test "hash-table-update! calls the default as a thunk" 11
    (let ((h (make-strong-eqv-hash-table)))
      (hash-table-update! h 'y (lambda (v) (+ v 1)) (lambda () 10))
      (hash-table-ref/default h 'y #f)))
(test "hash-table-update! on an existing key" 6
    (let ((h (make-strong-eqv-hash-table)))
      (hash-table-set! h 'x 5)
      (hash-table-update! h 'x (lambda (v) (+ v 1)))
      (hash-table-ref/default h 'x #f)))
(test "member accepts an optional compare procedure" '(5.0 6)
    (member 5 (list 1 2 5.0 6) =))
(test "assoc accepts an optional compare procedure" '(2 . b)
    (assoc 2.0 (list (cons 1 'a) (cons 2 'b)) =))
(test "reduce returns ridentity on an empty list" 0 (reduce + 0 '()))
(test "reduce folds over a nonempty list" 10 (reduce + 0 (list 1 2 3 4)))
(test "any returns the predicate's true value, not a bare #t" 4
    (any (lambda (x) (and (even? x) x)) '(1 3 4 5)))
(test "every returns the last predicate value, not a bare #t" 9
    (every (lambda (x) (* x x)) '(1 2 3)))
(test "any over multiple lists stops at the shortest" #f (any < '(1 2) '()))
(test "any/every on an empty list" '(#f . #t)
    (cons (any (lambda (x) x) '()) (every (lambda (x) x) '())))
(test "take/drop use (list k) order" '((1 2 3) (4 5))
    (list (take '(1 2 3 4 5) 3) (drop '(1 2 3 4 5) 3)))
(test "take-right/drop-right on a proper list" '((3) (1 2))
    (list (take-right '(1 2 3) 1) (drop-right '(1 2 3) 1)))
(test "take-right/drop-right accept an improper list" '((b c . d) (a))
    (list (take-right '(a b c . d) 2) (drop-right '(a b c . d) 2)))
(test "list-copy preserves an improper tail" '(1 2 . 3)
    (list-copy '(1 2 . 3)))
(test "last-pair accepts an improper list" '(b . c) (last-pair '(a b . c)))
(test "append! accepts a non-list final argument" '(1 2 . 3)
    (append! (list 1 2) 3))
(test "iota with start and step" '(10 12 14 16 18) (iota 5 10 2))
(test "iota default start and step" '(0 1 2 3 4) (iota 5))
(test "string-join with separator and prefix but no suffix" "[a,b"
    (string-join (list "a" "b") "," "["))
(test "string-join with separator, prefix, and suffix" "<a-b>"
    (string-join (list "a" "b") "-" "<" ">"))
(test "format ~r honors a radix parameter" "hex: ff decimal: 42 bin: 101"
    (format #f "hex: ~16r decimal: ~r bin: ~b" 255 42 5))

(test-section "Numeric semantics")
(test "eqv? on bignums" #t (eqv? (expt 10 30) (expt 10 30)))
(test "eqv? on rationals" #t (eqv? 1/2 1/2))
(test "eqv? on inexact" #t (eqv? 1.5 1.5))
(test "eqv? distinguishes exactness" #f (eqv? 1 1.0))
(test "round half to even" 2.0 (round 2.5))
(test "round rational half to even" 2 (round 5/2))
(test "exact rational floor" 333333333333333333333333333333
    (floor (/ (expt 10 30) 3)))
(test "mixed compare rational float" #f (= 1/10 0.1))
(test "inexact division by zero" #t (= (/ 1.0 0.0) (* 2 (/ 1.0 0.0))))
(test "quotient inexact contagion" 3.0 (quotient 7.0 2))
(test "string->number radix prefix" 255 (string->number "#xff"))
(test "string->number exact decimal" 3/2 (string->number "#e1.5"))
(test "radix rational literal" 1/2 #x1/2)
(test "numerator of inexact" 3.0 (numerator 1.5))
(test "rationalize contagion" #t (inexact? (rationalize .3 1/10)))
;; rational? used to only recognize the exact numeric representations,
;; missing that any finite inexact real is also rational (R7RS 6.2.5) -
;; only +inf.0/-inf.0/+nan.0 are real but not rational.
(test "rational? on finite inexact" #t (rational? 3.5))
(test "rational? on inexact integer" #t (rational? 2.0))
(test "rational? on infinity" #f (rational? +inf.0))
(test "rational? on negative infinity" #f (rational? -inf.0))
(test "rational? on nan" #f (rational? +nan.0))
;; make-rectangular used to collapse to just the real part whenever the
;; imaginary part was numerically zero, even if it was an *inexact* zero -
;; per R7RS/MIT, only an exact zero imaginary part collapses; an inexact
;; 0.0 produces a genuine (inexact) complex number.
(test "make-rectangular with inexact zero imaginary stays complex" #f
    (exact? (make-rectangular 3 0.0)))
(test "make-rectangular with exact zero imaginary collapses to real" #t
    (and (= 3 (make-rectangular 3 0)) (exact? (make-rectangular 3 0))))
(test "make-rectangular with inexact real, exact zero imaginary" 3.0
    (make-rectangular 3.0 0))

(test-section "Reader and writer round-trips")
(test "eof-object is not a symbol" #f (symbol? (eof-object)))
(test "eof-object? rejects the symbol" #f (eof-object? 'eof-object))
(test "float round-trip" #t
    (= 3.141592653589793
       (string->number (number->string 3.141592653589793))))
(test "string line continuation" "ab"
    "a\
b")
(test "numeric-looking symbol round-trip" '+inf.0-sym
    (string->symbol
     (string-append (symbol->string (string->symbol "+inf.0")) "-sym")))
(test "infinity literal" #t (> +inf.0 0))

(test-section "Inexactness contagion in min/max/gcd/lcm")
;; R7RS 6.2.6: if any argument is inexact the result is inexact, even when
;; the operand that wins the comparison (or survives a zero short-circuit)
;; is the exact one.
(test "min propagates inexactness" 1.0 (min 1 2.0))
(test "max propagates inexactness" 2.0 (max 1.0 2))
(test "min propagates from a later argument" 3.0 (min 5 3 4.0))
(test "min keeps the smaller operand, coerced" 0.5 (min 1e300 1/2))
(test "max over exact arguments stays exact" #t (exact? (max 1 2)))
(test "min over exact arguments stays exact" #t (exact? (min 1 2)))
(test "gcd propagates inexactness through its zero base case" 1.0 (gcd 0.0 1))
(test "gcd propagates inexactness from either side" 1.0 (gcd 1 0.0))
(test "gcd propagates inexactness generally" 2.0 (gcd 4.0 6))
(test "gcd over exact arguments stays exact" #t (exact? (gcd 4 6)))
(test "lcm propagates inexactness through its zero result" 0.0 (lcm 0.0 1))
(test "lcm propagates inexactness generally" 12.0 (lcm 4.0 6))
(test "lcm over exact arguments stays exact" #t (exact? (lcm 4 6)))
(test "contagion survives downstream rounding" #f
    (exact-integer? (round (max 1e-10 1.5 7/2))))

(test-section "case with => clauses")
;; R7RS 4.2.1 allows ((datum ...) => proc) and (else => proc).
(test "case => applies the procedure to the key" '(a a)
    (case 'a ((a) => (lambda (x) (list x x))) (else 'no)))
(test "case else => applies to the key" '(z)
    (case 'z ((a) 'no) (else => list)))
(test "case => only fires on a match" 'other
    (case 9 ((1) => list) (else 'other)))
(test "case => among ordinary clauses" 'plain
    (case 2 ((1) => list) ((2) 'plain) (else 'no)))
(test "case with no match and no else is unspecified but does not error" #t
    (begin (case 9 ((1) 'one)) #t))
(test "case key is evaluated once" 1
    (let ((n 0))
      (case (begin (set! n (+ n 1)) 2) ((2) n) (else 'no))))

(test-section "Handler returning from a non-continuable raise")
;; R7RS 6.11: the secondary exception is raised in the dynamic environment of
;; the handler, where the OUTER handler is installed - so it is catchable.
(test "guard catches a handler that falls off the end" 'caught
    (guard (e (#t 'caught))
      (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x)))))
(test "outer with-exception-handler sees the secondary exception" 'outer
    (call/cc
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k 'outer))
          (lambda ()
            (with-exception-handler (lambda (e) 'ret)
                                    (lambda () (raise 'x))))))))
(test "the secondary exception is an error object" #t
    (guard (e (#t (error-object? e)))
      (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x)))))
(test "a returning handler does not re-enter itself" 1
    (let ((calls 0))
      (guard (e (#t calls))
        (with-exception-handler
          (lambda (e) (set! calls (+ calls 1)) 'ret)
          (lambda () (raise 'x))))))
(test "raise-continuable still resumes with the handler value" 99
    (with-exception-handler (lambda (e) 99) (lambda () (raise-continuable 'c))))
(test "raise-continuable resumes mid-expression" 100
    (with-exception-handler (lambda (e) 99)
                            (lambda () (+ 1 (raise-continuable 'c)))))
(test "a C-level error reaches an enclosing guard" 'caught-car
    (guard (e (#t 'caught-car)) (car '())))
(test "nested guard without a matching clause propagates outward" 'outer
    (guard (e (#t 'outer))
      (guard (e ((string? e) 'nope)) (raise 'sym))))

(test-section "Exactness predicates reject non-numbers")
;; R7RS 6.2.6 defines these over numbers; answering #f would disguise a type
;; error as a plausible answer.
(test "exact? on a symbol is an error" 'err
    (guard (e (#t 'err)) (exact? 'a)))
(test "inexact? on a string is an error" 'err
    (guard (e (#t 'err)) (inexact? "s")))
(test "nan? on a symbol is an error" 'err
    (guard (e (#t 'err)) (nan? 'a)))
(test "finite? on a symbol is an error" 'err
    (guard (e (#t 'err)) (finite? 'a)))
(test "infinite? on a symbol is an error" 'err
    (guard (e (#t 'err)) (infinite? 'a)))
(test "exact? still answers for numbers" #t (exact? 1/2))
(test "inexact? still answers for numbers" #t (inexact? 1.0))
(test "nan? still answers for numbers" #t (nan? (/ 0. 0.)))
(test "finite? still answers for numbers" #t (finite? 1))
(test "infinite? still answers for numbers" #t (infinite? (/ 1. 0.)))
;; exact-integer?/exact-rational? are type predicates, not questions about a
;; number, so they keep answering #f.
(test "exact-integer? stays total" #f (exact-integer? 'a))
(test "exact-rational? stays total" #f (exact-rational? 'a))

(test-section "asin/acos outside [-1, 1]")
;; sqrt, log and expt already return complex results rather than NaN; asin
;; and acos now agree, and accept complex arguments like sin/cos/tan do.
(test "asin 2 is complex, not NaN" #t (not (real? (asin 2))))
(test "acos 2 is complex, not NaN" #t (not (real? (acos 2))))
(test "asin round-trips through sin" #t
    (< (magnitude (- (sin (asin 2)) 2)) 1e-9))
(test "acos round-trips through cos" #t
    (< (magnitude (- (cos (acos 2)) 2)) 1e-9))
(test "asin -2 round-trips through sin" #t
    (< (magnitude (- (sin (asin -2)) -2)) 1e-9))
(test "asin real part matches MIT for x > 1" #t
    (< (abs (- (real-part (asin 2)) 1.5707963267948966)) 1e-12))
(test "asin stays real inside the domain" #t (real? (asin 0.5)))
(test "acos stays real inside the domain" #t (real? (acos 0.5)))
(test "asin of a complex argument round-trips" #t
    (< (magnitude (- (sin (asin (make-rectangular 1 1)))
                     (make-rectangular 1 1)))
       1e-9))
(test "asin propagates NaN" #t (nan? (asin +nan.0)))

(test-section "Non-tail recursion depth")
;; The bytecode VM used to cap the call stack at 64K frames, which failed on
;; an ordinary recursive list builder that the CPS interpreter handled fine.
(test "100k-deep non-tail recursion" 100000
    (let ()
      (define (build n) (if (= n 0) '() (cons n (build (- n 1)))))
      (length (build 100000))))
(test "200k-deep non-tail recursion" 200000
    (let ()
      (define (build n) (if (= n 0) '() (cons n (build (- n 1)))))
      (length (build 200000))))
(test "deep recursion still returns the right elements" '(3 2 1)
    (let ()
      (define (build n) (if (= n 0) '() (cons n (build (- n 1)))))
      (list-tail (build 100000) 99997)))

(test-section "Tail recursion modulo cons")
;; (cons E (self a ...)) in tail position compiles to a loop that builds the
;; list forwards and mutates the last cell's cdr, so depth is bounded by
;; memory rather than by VM frames.
(define (trmc-build n)
  (let loop ((k n))
    (if (= k 0) '() (cons k (loop (- k 1))))))
(test "1.2M elements, past the 1M frame ceiling" 1200000
    (length (trmc-build 1200000)))
(test "elements come out in order" '(5 4 3 2 1) (trmc-build 5))
(test "empty result" '() (trmc-build 0))
(test "single element" '(1) (trmc-build 1))
(test "deep result has the right elements at the end" '(3 2 1)
    (list-tail (trmc-build 100000) 99997))

;; A base case that is not a list leaves a dotted tail, which the return
;; splices in unchanged rather than treating as the end of a proper list.
(define (trmc-dotted n)
  (let loop ((k n))
    (if (= k 0) 'end (cons k (loop (- k 1))))))
(test "non-list base case becomes a dotted tail" '(2 1 . end) (trmc-dotted 2))
(test "non-list base case with no elements" 'end (trmc-dotted 0))

;; Same shape with cons rebound, which declines the transform: the two must
;; agree everywhere, not just on the happy path.
(define (trmc-plain n mycons)
  (let loop ((k n))
    (if (= k 0) '() (mycons k (loop (- k 1))))))
(test "transformed and untransformed agree for 0..40" #t
    (let loop ((i 0))
      (if (> i 40)
          #t
          (if (equal? (trmc-build i) (trmc-plain i cons)) (loop (+ i 1)) #f))))

;; The transform is only sound while the half-built list is unobservable.
;; A capture in the element expression makes it observable, so the transform
;; must decline - and the way to tell is that re-entering the continuation
;; must not rewrite the list the first run already returned.
(define trmc-k #f)
(define trmc-saved #f)
(define (trmc-cap n)
  (let loop ((j n))
    (if (= j 0)
        '()
        (cons (call-with-current-continuation
               (lambda (c) (if (= j 1) (set! trmc-k c)) j))
              (loop (- j 1))))))
(define trmc-reentry
  (call-with-current-continuation
   (lambda (return)
     (let ((lst (trmc-cap 3)))
       (if (not trmc-saved)
           (begin (set! trmc-saved lst)
                  (let ((k trmc-k)) (set! trmc-k #f) (k 99))))
       (return (list trmc-saved lst))))))
(test "re-entering a captured continuation rebuilds the list" '(3 2 99)
    (cadr trmc-reentry))
(test "the first result is not mutated by the second run" '(3 2 1)
    (car trmc-reentry))

(test-section "Tail recursion modulo append")
;; append already copies every argument but the last and shares that one,
;; which is the accumulator's shape exactly. Splicing the copies outermost
;; first does the same total work; a left fold over append would recopy the
;; whole prefix each iteration and be quadratic.
(define (trmc-spans n)
  (let loop ((k n))
    (if (= k 0) '() (append (list k k) (loop (- k 1))))))
(test "append-modulo builds in order" '(3 3 2 2 1 1) (trmc-spans 3))
(test "1M elements from 500k appends" 1000000 (length (trmc-spans 500000)))
(test "empty operands contribute nothing" '(6 4 2)
    (let loop ((k 6))
      (if (= k 0) '() (append (if (even? k) (list k) '()) (loop (- k 1))))))
(test "n-ary append splices operands left to right" '(3 -3 2 -2 1 -1 end)
    (let loop ((k 3))
      (if (= k 0) '(end)
          (append (list k) (list (- k)) (loop (- k 1))))))

;; The last argument is shared, not copied - the one place append's identity
;; is observable through eq?.
(define trmc-shared-tail '(9 9))
(define (trmc-shared n)
  (let loop ((k n))
    (if (= k 0) trmc-shared-tail (append (list k) (loop (- k 1))))))
(test "the base case value is shared, not copied" #t
    (eq? (list-tail (trmc-shared 3) 3) trmc-shared-tail))
(test "append-modulo result is otherwise correct" '(3 2 1 9 9)
    (trmc-shared 3))
(test "an improper operand is still an error" 'err
    (guard (e (#t 'err))
      (let loop ((k 2)) (if (= k 0) '() (append '(1 . 2) (loop (- k 1)))))))

(test-section "Tail recursion modulo a general operator")
;; Where there is no hole to mutate - string-append, arithmetic - or where the
;; body could capture a continuation, the pending operations are stacked
;; functionally and replayed at the return instead. Prepending never mutates,
;; so this mode needs no restriction on the body; and for a pure operator
;; nothing about the evaluation order changes, since the operands were already
;; evaluated outermost-first and the operator still applies innermost-first.

;; Depths here are modest on purpose. This file runs in both engines, and the
;; CPS interpreter has no such transform - it really does recurse, at about
;; 17s per 200k levels. That the transform clears the VM's frame ceiling is
;; asserted in test_eval, which is VM-only and can afford the depth.

;; The element is a general call, which the mutating mode cannot accept.
(define (trmc-scale x) (* x 10))
(define (trmc-walk n)
  (let loop ((k n))
    (if (= k 0) '() (cons (trmc-scale k) (loop (- k 1))))))
(test "general element expression" '(50 40 30 20 10) (trmc-walk 5))
(test "100k elements with a general element" 100000 (length (trmc-walk 100000)))

;; string-append: quadratic as a left fold, one pass as a replay.
(define (trmc-spell n)
  (let loop ((k n))
    (if (= k 0) "" (string-append (number->string k) "," (loop (- k 1))))))
(test "string-append modulo" "5,4,3,2,1," (trmc-spell 5))
(test "50k string-append levels" 288894
    (string-length (trmc-spell 50000)))

;; Arithmetic over exact integers.
(define (trmc-total n)
  (let loop ((k n))
    (if (= k 0) 0 (+ k (loop (- k 1))))))
(test "sum of 1..10" 55 (trmc-total 10))
(test "sum of 1..100000" 5000050000 (trmc-total 100000))

;; Subtraction is not associative, and does not have to be: the replay applies
;; the operator innermost-first, exactly as the recursion would have.
(define (trmc-alt n)
  (let loop ((k n))
    (if (= k 0) 0 (- k (loop (- k 1))))))
(test "non-associative operator keeps its nesting" 3 (trmc-alt 5))
(test "non-associative operator, deep" 50000 (trmc-alt 100000))

;; A tail call to another procedure becomes an ordinary call inside a
;; transformed body, so the return that replays the operations still runs.
(define (trmc-fin) '(done))
(define (trmc-calls n)
  (let loop ((k n))
    (if (= k 0) (trmc-fin) (cons k (loop (- k 1))))))
(test "foreign tail call in the base case" '(3 2 1 done) (trmc-calls 3))

;; The property the mutating mode cannot offer: re-entering a continuation
;; captured in the loop rebuilds from that continuation's own accumulator and
;; leaves the first result alone.
(define trmc-fk #f)
(define trmc-fsaved #f)
(define (trmc-fcap n)
  (let loop ((j n))
    (if (= j 0)
        '()
        (cons (trmc-scale (call-with-current-continuation
                           (lambda (c) (if (= j 1) (set! trmc-fk c)) j)))
              (loop (- j 1))))))
(define trmc-freentry
  (call-with-current-continuation
   (lambda (return)
     (let ((lst (trmc-fcap 3)))
       (if (not trmc-fsaved)
           (begin (set! trmc-fsaved lst)
                  (let ((c trmc-fk)) (set! trmc-fk #f) (c 99))))
       (return (list trmc-fsaved lst))))))
(test "re-entry rebuilds independently" '(30 20 990) (cadr trmc-freentry))
(test "the first result is untouched" '(30 20 10) (car trmc-freentry))

(test-section "TRMC under cond, and/or and let")
;; cond pushes no environment frame, so its clause bodies sit at the same
;; depth as an if's branches.
(define (trmc-cond n)
  (let loop ((k n))
    (cond ((= k 0) '())
          (else (cons k (loop (- k 1)))))))
(test "cond clause bodies" '(3 2 1) (trmc-cond 3))
(test "cond, past the frame ceiling" 1200000 (length (trmc-cond 1200000)))
(test "cond with several clauses" '(3 -2 1)
    (let loop ((k 3))
      (cond ((= k 0) '())
            ((even? k) (cons (- k) (loop (- k 1))))
            (else (cons k (loop (- k 1)))))))

;; Only the last operand of and/or is in tail position.
(test "and contributes its last operand" '(2 1)
    (let loop ((k 2)) (and #t (if (= k 0) '() (cons k (loop (- k 1)))))))
(test "or short-circuits to the accumulator's tail" '(2 1 . #f)
    (let loop ((k 2)) (or (if (= k 0) #f (cons k (loop (- k 1)))) #f)))

;; A let frame has to come off before the loop jump; leaking one per
;; iteration would exhaust the environment long before this finishes.
(define (trmc-let n)
  (let loop ((k n))
    (if (= k 0) '()
        (let ((d (* k 2))) (cons d (loop (- k 1)))))))
(test "let inside the loop body" '(6 4 2) (trmc-let 3))
(test "let inside the loop body, deep" 200000 (length (trmc-let 200000)))

(test-section "Case-insensitive string comparison is n-ary")
;; R7RS 6.7 gives these two-or-more arguments, like their case-sensitive
;; siblings. stdlib redefined them as strictly binary, shadowing the C
;; primitive that already handled the n-ary case with the same foldcase.
(test "string-ci=? with three arguments" #t (string-ci=? "a" "A" "a"))
(test "string-ci<? with three arguments" #t (string-ci<? "a" "B" "c"))
(test "string-ci>? with three arguments" #t (string-ci>? "c" "B" "a"))
(test "string-ci<=? with three arguments" #t (string-ci<=? "a" "A" "b"))
(test "string-ci>=? with three arguments" #t (string-ci>=? "b" "B" "a"))
(test "string-ci=? rejects a lone argument" 'err
    (guard (e (#t 'err)) (string-ci=? "a")))
(test "a non-matching argument still answers #f" #f (string-ci=? "a" "A" "b"))
;; Folding stays full Unicode, not ASCII - all of these match MIT.
(test "sharp s folds to ss" #t (string-ci=? "Stra\x00df;e" "STRASSE"))
(test "final sigma folds with sigma" #t
    (string-ci=? "\x3a3;\x38a;\x3a3;\x3a5;\x3a6;\x39f;\x3a3;"
                 "\x3c3;\x3af;\x3c3;\x3c5;\x3c6;\x3bf;\x3c2;"))
(test "dotted capital I does not fold to i" #f (string-ci=? "\x130;" "i"))

(test-section "Internal defines are local and see their enclosing scope")
;; Both of these were VM-only failures: with stack locals the function has no
;; environment frame, so an internal define landed in the closure's
;; environment - the global one for a top-level procedure. The CPS
;; interpreter and MIT were always right.
(define (trmc-idef-outer n)
  (define (idef-inner x) (* x 2))
  (idef-inner n))
(test "internal define computes" 42 (trmc-idef-outer 21))
(test "internal define does not escape its body" 'unbound
    (guard (e (#t 'unbound)) (idef-inner 5)))

(define (idef-capture n)
  (define (helper x) (* x n))
  (helper 2))
(test "internal define sees an enclosing parameter" 42 (idef-capture 21))

;; The same through the (define name (lambda ...)) spelling, which worked
;; before only because the walk could see the lambda.
(define (idef-lambda-form n)
  (define helper (lambda (x) (* x n)))
  (helper 2))
(test "internal define written as a lambda" 42 (idef-lambda-form 21))

;; Several internal defines, mutually recursive, all reading a parameter.
(define (idef-mutual n)
  (define (even-step k) (if (= k 0) n (odd-step (- k 1))))
  (define (odd-step k) (if (= k 0) (- n) (even-step (- k 1))))
  (even-step 4))
(test "mutually recursive internal defines" 7 (idef-mutual 7))
(test "neither escapes" 'unbound
    (guard (e (#t 'unbound)) (even-step 1)))

;; A body whose defines arrive inside a begin, which R7RS splices.
(define (idef-in-begin n)
  (begin (define (spliced x) (+ x n)))
  (spliced 1))
(test "internal define spliced from a begin" 43 (idef-in-begin 42))

;; letrec still defines into its own frame, so it keeps stack locals.
(define (idef-letrec n)
  (letrec ((go (lambda (k) (if (= k 0) n (go (- k 1))))))
    (go 3)))
(test "letrec is unaffected" 9 (idef-letrec 9))

(test-section "Proper tail calls survive the TRMC transform")
;; trmc-tc-a qualifies for the transform, and its base case tail-calls
;; trmc-tc-b, which tail-calls back. R7RS requires the cycle to run in
;; constant space. Compiling that call as an ordinary call would grow the
;; stack once per round trip, not by the one frame it looks like.
(define (trmc-tc-b n) (if (= n 0) 'done (trmc-tc-a (- n 1))))
(define (trmc-tc-a n)
  (let loop ((k 0))
    (if (= k 0) (trmc-tc-b n) (cons k (loop (- k 1))))))
(test "3M-deep mutual recursion through a transformed body" 'done
    (trmc-tc-a 3000000))

;; With something already accumulated the value has to come back to be
;; folded in, so the result still has to be right.
(define (trmc-tc-c n)
  (let loop ((k n))
    (if (= k 0) (trmc-tc-tail) (cons k (loop (- k 1))))))
(define (trmc-tc-tail) '(end))
(test "a pending accumulator still folds the callee's value in" '(3 2 1 end)
    (trmc-tc-c 3))
(test "and deeply" 200001 (length (trmc-tc-c 200000)))

(test-section "Macro expansion guard is a depth, not a total")
;; The CPS interpreter's expansion guard used to be a cumulative cap: any
;; single top-level form that expanded more than 1000 macro uses in total
;; failed with "macro expansion exceeded maximum depth", even with no nesting
;; at all. eval in a loop is the natural trigger.
(define-syntax guard-probe-macro
  (syntax-rules () ((_ x) (let ((tmp x)) (+ tmp 1)))))
(test "1500 evals of a macro call in one form" 1500
    (let ((env (interaction-environment)))
      (let loop ((i 0) (acc 0))
        (if (= i 1500)
            acc
            (loop (+ i 1) (+ acc (- (eval '(guard-probe-macro 0) env) 0)))))))
;; ...while a genuinely infinite expansion must still be caught, not hang.
(define-syntax runaway-probe-macro
  (syntax-rules () ((_) (runaway-probe-macro))))
(test "infinite macro expansion is still caught" 'caught
    (guard (e (#t 'caught)) (runaway-probe-macro)))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(newline)
(display "========================================")
(newline)
(display "R5RS Tests: ")
(display (+ test-pass test-fail))
(display ", Passed: ")
(display test-pass)
(display ", Failed: ")
(display test-fail)
(newline)
(if (= test-fail 0)
    (display "All R5RS tests passed!")
    (begin
      (display "Some tests failed!")))
(newline)
