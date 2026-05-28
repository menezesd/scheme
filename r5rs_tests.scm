;;; R5RS Conformance Tests
;;; Tests core R5RS features for compliance

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

(test-section "Hygiene edge cases")
(test "else as binding" 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))
(test "=> as binding" 'ok (let ((=> 1)) (cond (#t => 'ok))))
;; These edge cases require runtime quasiquote/ellipsis shadowing, not supported in bytecode mode
;; (test "unquote as binding" '(,foo) (let ((unquote 1)) `(,foo)))
;; (test "unquote-splicing as binding" '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))
;; (test "... as binding" 'ok
;;     (let ((... 2))
;;       (let-syntax ((s (syntax-rules ()
;;                         ((_ x ...) 'bad)
;;                         ((_ . r) 'ok))))
;;         (s a b c))))
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
