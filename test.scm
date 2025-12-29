; Test Suite for Scheme Interpreter
; Run with: ./lisp < test.scm

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "  FAIL: ")
        (display name)
        (newline)
        (display "    expected: ")
        (write expected)
        (newline)
        (display "    got:      ")
        (write actual)
        (newline))))

(define (section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ============================================================================
;;; Arithmetic
;;; ============================================================================

(section "Arithmetic")

(test "addition" 6 (+ 1 2 3))
(test "subtraction" 5 (- 10 3 2))
(test "multiplication" 24 (* 2 3 4))
(test "division" 4 (/ 12 3))
; Note: (/ 1 2) returns the integer 0 in integer division, not 1/2
(test "division to rational" 1/3 (/ 1 3))
(test "modulo" 1 (modulo 10 3))
(test "remainder" 1 (remainder 10 3))
(test "quotient" 3 (quotient 10 3))
(test "abs positive" 5 (abs 5))
(test "abs negative" 5 (abs -5))
(test "gcd" 6 (gcd 12 18))
(test "lcm" 36 (lcm 12 18))

;;; ============================================================================
;;; Numeric predicates
;;; ============================================================================

(section "Numeric Predicates")

(test "zero? true" #t (zero? 0))
(test "zero? false" #f (zero? 1))
(test "positive? true" #t (positive? 5))
(test "positive? false" #f (positive? -5))
(test "negative? true" #t (negative? -5))
(test "negative? false" #f (negative? 5))
(test "odd? true" #t (odd? 3))
(test "odd? false" #f (odd? 4))
(test "even? true" #t (even? 4))
(test "even? false" #f (even? 3))

;;; ============================================================================
;;; Comparisons
;;; ============================================================================

(section "Comparisons")

(test "< true" #t (< 1 2 3))
(test "< false" #f (< 1 3 2))
(test "> true" #t (> 3 2 1))
(test "<= true" #t (<= 1 2 2 3))
(test ">= true" #t (>= 3 2 2 1))
(test "= true" #t (= 2 2 2))
(test "= false" #f (= 2 2 3))

;;; ============================================================================
;;; List operations
;;; ============================================================================

(section "List Operations")

(test "cons" '(1 . 2) (cons 1 2))
(test "car" 1 (car '(1 2 3)))
(test "cdr" '(2 3) (cdr '(1 2 3)))
(test "list" '(1 2 3) (list 1 2 3))
(test "length" 3 (length '(1 2 3)))
(test "append" '(1 2 3 4) (append '(1 2) '(3 4)))
(test "reverse" '(3 2 1) (reverse '(1 2 3)))
(test "list-ref" 'b (list-ref '(a b c) 1))
(test "list-tail" '(c d) (list-tail '(a b c d) 2))

;;; ============================================================================
;;; Predicates
;;; ============================================================================

(section "Type Predicates")

(test "null? true" #t (null? '()))
(test "null? false" #f (null? '(1)))
(test "pair? true" #t (pair? '(1 . 2)))
(test "pair? false" #f (pair? 5))
(test "list? true" #t (list? '(1 2 3)))
(test "list? false" #f (list? '(1 . 2)))
(test "symbol? true" #t (symbol? 'foo))
(test "symbol? false" #f (symbol? 5))
(test "number? true" #t (number? 42))
(test "string? true" #t (string? "hello"))
(test "char? true" #t (char? #\a))
(test "vector? true" #t (vector? #(1 2 3)))
(test "procedure? true" #t (procedure? +))
(test "boolean? true" #t (boolean? #t))
(test "boolean? false" #f (boolean? 0))

;;; ============================================================================
;;; Equality
;;; ============================================================================

(section "Equality")

(test "eq? same symbol" #t (eq? 'a 'a))
(test "eq? different" #f (eq? 'a 'b))
(test "eqv? numbers" #t (eqv? 42 42))
(test "equal? lists" #t (equal? '(1 2 3) '(1 2 3)))
(test "equal? nested" #t (equal? '(1 (2 3)) '(1 (2 3))))

;;; ============================================================================
;;; Membership and association
;;; ============================================================================

(section "Membership and Association")

(test "memq found" '(b c) (memq 'b '(a b c)))
(test "memq not found" #f (memq 'd '(a b c)))
(test "member found" '((2) 3) (member '(2) '(1 (2) 3)))
(test "assq found" '(b . 2) (assq 'b '((a . 1) (b . 2) (c . 3))))
(test "assq not found" #f (assq 'd '((a . 1) (b . 2))))
(test "assoc found" '((1) . a) (assoc '(1) '(((0) . z) ((1) . a))))

;;; ============================================================================
;;; Control flow
;;; ============================================================================

(section "Control Flow")

(test "if true" 1 (if #t 1 2))
(test "if false" 2 (if #f 1 2))
(test "cond" 'big (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'big)))
(test "and true" 3 (and 1 2 3))
(test "and false" #f (and 1 #f 3))
(test "or true" 1 (or #f 1 2))
(test "or false" #f (or #f #f #f))
(test "not true" #t (not #f))
(test "not false" #f (not #t))

;;; ============================================================================
;;; Let forms
;;; ============================================================================

(section "Let Forms")

(test "let" 3 (let ((x 1) (y 2)) (+ x y)))
(test "let*" 3 (let* ((x 1) (y (+ x 1))) (+ x y)))
(test "letrec" 120 (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5)))
(test "named let" 55 (let loop ((n 10) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n)))))

;;; ============================================================================
;;; Lambda and closures
;;; ============================================================================

(section "Lambda and Closures")

(test "lambda" 5 ((lambda (x) (+ x 2)) 3))
(test "closure" 10 (let ((x 5)) ((lambda (y) (+ x y)) 5)))
(test "higher order" 9 ((lambda (f) (f 3)) (lambda (x) (* x x))))
(test "varargs" '(1 2 3) ((lambda args args) 1 2 3))
(test "rest args" '(2 3) ((lambda (x . rest) rest) 1 2 3))

;;; ============================================================================
;;; Strings
;;; ============================================================================

(section "Strings")

(test "string-length" 5 (string-length "hello"))
(test "string-ref" #\e (string-ref "hello" 1))
(test "string-append" "helloworld" (string-append "hello" "world"))
(test "substring" "ell" (substring "hello" 1 4))
(test "string->list" '(#\a #\b #\c) (string->list "abc"))
(test "list->string" "abc" (list->string '(#\a #\b #\c)))
(test "string->symbol" 'hello (string->symbol "hello"))
(test "symbol->string" "hello" (symbol->string 'hello))

;;; ============================================================================
;;; Characters
;;; ============================================================================

(section "Characters")

(test "char->integer" 97 (char->integer #\a))
(test "integer->char" #\a (integer->char 97))
(test "char<?" #t (char<? #\a #\b))
(test "char-alphabetic?" #t (char-alphabetic? #\a))
(test "char-numeric?" #t (char-numeric? #\5))
(test "char-whitespace?" #t (char-whitespace? #\space))
(test "char-upcase" #\A (char-upcase #\a))
(test "char-downcase" #\a (char-downcase #\A))

;;; ============================================================================
;;; Vectors
;;; ============================================================================

(section "Vectors")

(test "make-vector" #(0 0 0) (make-vector 3 0))
(test "vector" #(1 2 3) (vector 1 2 3))
(test "vector-length" 3 (vector-length #(1 2 3)))
(test "vector-ref" 2 (vector-ref #(1 2 3) 1))
(test "vector->list" '(1 2 3) (vector->list #(1 2 3)))
(test "list->vector" #(1 2 3) (list->vector '(1 2 3)))

;;; ============================================================================
;;; Higher order functions
;;; ============================================================================

(section "Higher Order Functions")

(test "map" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "apply" 6 (apply + '(1 2 3)))
(test "for-each" 6 (let ((sum 0)) (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3)) sum))

;;; ============================================================================
;;; String ports
;;; ============================================================================

(section "String Ports")

(test "open-output-string" #t (output-port? (open-output-string)))
(test "get-output-string" "hello"
      (let ((p (open-output-string)))
        (display "hello" p)
        (get-output-string p)))
(test "open-input-string" #t (input-port? (open-input-string "test")))
(test "read-char from string" #\h
      (let ((p (open-input-string "hello")))
        (read-char p)))
(test "call-with-output-string" "test"
      (call-with-output-string (lambda (p) (display "test" p))))
(test "call-with-input-string" #\a
      (call-with-input-string "abc" (lambda (p) (read-char p))))
(test "string-port?" #t (string-port? (open-output-string)))
(test "string-port? file" #f (string-port? (current-output-port)))

;;; ============================================================================
;;; Case and Do
;;; ============================================================================

(section "Case and Do")

(test "case match" 'composite
      (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
(test "case else" 'other
      (case 10 ((1 2) 'small) ((3 4) 'medium) (else 'other)))
(test "do loop" 15
      (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) (+ sum i))))

;;; ============================================================================
;;; Delay/Force
;;; ============================================================================

(section "Delay and Force")

(test "delay/force" 10 (force (delay (+ 5 5))))
(test "force memoizes" #t
      (let* ((count 0)
             (p (delay (begin (set! count (+ count 1)) count))))
        (force p)
        (force p)
        (= count 1)))

;;; ============================================================================
;;; Bignums
;;; ============================================================================

(section "Bignums")

(test "bignum add" 20000000000000000000 (+ 10000000000000000000 10000000000000000000))
(test "bignum mult" 100000000000000000000000000000000000000 (* 10000000000000000000 10000000000000000000))
(test "factorial 50" 30414093201713378043612608166064768844377641568960512000000000000
      (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 50)))

;;; ============================================================================
;;; Multiple values
;;; ============================================================================

(section "Multiple Values")

(test "values/call-with-values" 3
      (call-with-values (lambda () (values 1 2)) +))

;;; ============================================================================
;;; List Utilities
;;; ============================================================================

(section "List Utilities")

(test "filter" '(2 4 6) (filter even? '(1 2 3 4 5 6)))
(test "remove" '(1 3 5) (remove even? '(1 2 3 4 5 6)))
(test "find" 4 (find even? '(1 3 4 5 6)))
(test "find not found" #f (find even? '(1 3 5 7)))
(test "any true" #t (any even? '(1 3 4 5)))
(test "any false" #f (any even? '(1 3 5 7)))
(test "every true" #t (every odd? '(1 3 5 7)))
(test "every false" #f (every odd? '(1 2 3 4)))
(test "count" 3 (count even? '(1 2 3 4 5 6)))
(test "fold" 15 (fold + 0 '(1 2 3 4 5)))
(test "fold-right" '(1 2 3 4 5) (fold-right cons '() '(1 2 3 4 5)))
(test "take" '(1 2 3) (take 3 '(1 2 3 4 5)))
(test "drop" '(4 5) (drop 3 '(1 2 3 4 5)))
(test "partition" '((2 4 6) (1 3 5)) (partition even? '(1 2 3 4 5 6)))
(test "zip" '((1 a) (2 b) (3 c)) (zip '(1 2 3) '(a b c)))
(test "flatten" '(1 2 3 4 5) (flatten '(1 (2 (3 4) 5))))
(test "last" 5 (last '(1 2 3 4 5)))
(test "iota" '(0 1 2 3 4) (iota 5))
(test "range" '(2 3 4) (range 2 5))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(newline)
(display "========================================")
(newline)
(display "Tests: ")
(display test-count)
(display ", Passed: ")
(display pass-count)
(display ", Failed: ")
(display fail-count)
(newline)

;;; ============================================================================
;;; Continuations (call/cc)
;;; ============================================================================

(section "Continuations")

; Simple early exit
(test "call/cc early exit"
      10
      (+ 1 (call/cc (lambda (k) (+ 2 (k 9))))))

; Continuation captures the computation context
(test "call/cc captures context"
      15
      (+ 5 (call/cc (lambda (k) (k 10)))))

; Continuation ignores code after k is called
(test "call/cc aborts"
      10
      (call/cc (lambda (k)
                 (k 10)
                 (error "should not reach here"))))

; Escape from loop
(test "call/cc escape loop"
      5
      (call/cc
        (lambda (exit)
          (let loop ((n 0))
            (if (= n 5)
                (exit n)
                (loop (+ n 1)))))))

; Exception-like behavior
(define (safe-div a b)
  (call/cc
    (lambda (return)
      (if (= b 0)
          (return 'error)
          (/ a b)))))

(test "call/cc exception div ok" 5 (safe-div 10 2))
(test "call/cc exception div zero" 'error (safe-div 10 0))

; Simple amb-style backtracking (limited)
(define *fail* #f)

(define (choose lst)
  (if (null? lst)
      (*fail* 'fail)
      (call/cc
        (lambda (cc)
          (let ((old-fail *fail*))
            (set! *fail*
                  (lambda (x)
                    (set! *fail* old-fail)
                    (cc (choose (cdr lst)))))
            (car lst))))))

(define (require pred)
  (if (not pred) (*fail* 'fail)))

; Find a pair where a < b and a + b = 7
(define amb-result
  (call/cc
    (lambda (top-exit)
      (set! *fail* (lambda (x) (top-exit 'no-solution)))
      (let ((a (choose '(1 2 3 4 5)))
            (b (choose '(1 2 3 4 5))))
        (require (< a b))
        (require (= (+ a b) 7))
        (list a b)))))

; Verify result satisfies constraints
(test "amb-style choose valid"
      #t
      (and (pair? amb-result)
           (< (car amb-result) (cadr amb-result))
           (= 7 (+ (car amb-result) (cadr amb-result)))))

; Generator-style producer/consumer
(define (make-counter max)
  (let ((return #f)
        (count 0))
    (lambda ()
      (call/cc
        (lambda (k)
          (set! return k)
          (let loop ()
            (if (> count max)
                (return 'done)
                (call/cc
                  (lambda (next)
                    (set! return next)
                    (let ((c count))
                      (set! count (+ count 1))
                      (k c)))))
            (loop)))))))

(define gen (make-counter 3))
(test "generator 0" 0 (gen))
(test "generator 1" 1 (gen))
(test "generator 2" 2 (gen))
(test "generator 3" 3 (gen))
(test "generator done" 'done (gen))

;;; ============================================================================
;;; Continuation Stress Tests (GC + multi-shot continuations)
;;; ============================================================================

(section "Continuation Stress")

; Full amb implementation with heavy multi-shot reuse
(define fail-cont #f)

(define (fail)
  (if fail-cont
      (fail-cont 'fail)
      #f))

(define (amb . choices)
  (call/cc
   (lambda (return-choice)
     (let try ((remaining choices))
       (if (null? remaining)
           (fail)
           (call/cc
            (lambda (save-fail)
              (set! fail-cont (lambda (_) (save-fail 'next)))
              (return-choice (car remaining)))))))))

; Stress test: 100 amb iterations
(define amb-count 0)
(call/cc
 (lambda (escape)
   (set! fail-cont (lambda (_) (escape 'done)))
   (let loop ()
     (let ((x (amb 1 2 3 4 5)))
       (set! amb-count (+ amb-count 1))
       (if (>= amb-count 100)
           (escape 'done)
           (fail))))))

(test "amb stress 100 iterations" 100 amb-count)

; Generator implementation with multi-shot continuations
(define (make-generator proc)
  (let ((resume-point #f)
        (caller-cont #f))
    (lambda ()
      (call/cc
       (lambda (caller)
         (set! caller-cont caller)
         (if resume-point
             (resume-point 'continue)
             (begin
               (proc (lambda (value)
                       (call/cc
                        (lambda (k)
                          (set! resume-point k)
                          (caller-cont value)))))
               (caller-cont #f))))))))

(define (range-gen start end)
  (make-generator
   (lambda (yield)
     (let loop ((i start))
       (if (<= i end)
           (begin
             (yield i)
             (loop (+ i 1))))))))

; Stress test: generator with 50 iterations
(define gen-stress (range-gen 1 50))
(define gen-sum 0)
(let loop ()
  (let ((val (gen-stress)))
    (if val
        (begin
          (set! gen-sum (+ gen-sum val))
          (loop)))))

(test "generator stress sum 1-50" 1275 gen-sum)

;;; ============================================================================
;;; Summary
;;; ============================================================================

(if (= fail-count 0)
    (display "All tests passed!")
    (display "SOME TESTS FAILED!"))
(newline)
