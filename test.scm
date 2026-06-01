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
(test "for-each multiple stops at shortest" '(5 7)
      (let ((seen '()))
        (for-each (lambda (x y) (set! seen (cons (+ x y) seen)))
                  '(1 2 3)
                  '(4 5))
        (reverse seen)))

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
(test "with-output-to-string" "test"
      (with-output-to-string (display "test")))
(test "with-input-from-string" #\a
      (with-input-from-string "abc" (read-char)))
(test "with-output-to-string restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string))
            (escape #f))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (set! escape k)
            (with-output-to-string (escape #t))))
        (display "after")
        (set-current-output-port! old)
        (get-output-string p)))
(test "with-input-from-string restores on continuation escape" #\z
      (let ((old (current-input-port))
            (p (open-input-string "z"))
            (escape #f))
        (set-current-input-port! p)
        (let ((result
               (begin
                 (call/cc
                   (lambda (k)
                     (set! escape k)
                     (with-input-from-string "abc" (escape #t))))
                 (read-char))))
          (set-current-input-port! old)
          result)))
(test "string-port?" #t (string-port? (open-output-string)))
(test "string-port? file" #f (string-port? (current-output-port)))

;;; ============================================================================
;;; File Port Dynamic Extents
;;; ============================================================================

(section "File Port Dynamic Extents")

(test "call-with-output-file closes on continuation escape" #\x
      (begin
        (call/cc
          (lambda (k)
            (call-with-output-file
              "/tmp/vesper-call-with-output-escape-test.txt"
              (lambda (p)
                (display "x" p)
                (k #t)))))
        (call-with-input-file
          "/tmp/vesper-call-with-output-escape-test.txt"
          (lambda (p) (read-char p)))))

(test "with-output-to-file restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string)))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (with-output-to-file
              "/tmp/vesper-with-output-escape-test.txt"
              (lambda () (k #t)))))
        (display "after")
        (set-current-output-port! old)
        (get-output-string p)))

;;; ============================================================================
;;; Case and Do
;;; ============================================================================

(section "Case and Do")

(test "case match" 'composite
      (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
(test "case else" 'other
      (case 10 ((1 2) 'small) ((3 4) 'medium) (else 'other)))
(test "case evaluates key once" 1
      (let ((count 0))
        (case (begin (set! count (+ count 1)) 'c)
          ((a) 'a)
          ((b) 'b)
          ((c) count)
          (else 'other))))
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
(test "promise? delay" #t (promise? (delay 1)))
(test "promise? non-promise" #f (promise? (lambda () 1)))
(test "make-promise" 42 (force (make-promise 42)))
(test "make-promise preserves promise" #t
      (let ((p (delay 3)))
        (eq? p (make-promise p))))
(test "delay-force" 9 (force (delay-force (delay (+ 4 5)))))
(test "eof-object" #t (eof-object? (eof-object)))

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
(test "call/cc continuation accepts multiple values" '(1 2)
      (call-with-values
        (lambda () (call/cc (lambda (k) (k 1 2))))
        list))
(test "call/cc continuation accepts zero values" '()
      (call-with-values
        (lambda () (call/cc (lambda (k) (k))))
        list))
(test "dynamic-wind continuation forwards multiple values" '((a b) (before after))
      (let ((log '()))
        (list
          (call-with-values
            (lambda ()
              (dynamic-wind
                (lambda () (set! log (cons 'before log)))
                (lambda ()
                  (call/cc
                    (lambda (k)
                      (k 'a 'b))))
                (lambda () (set! log (cons 'after log)))))
            list)
          (reverse log))))

(define-values (defined-a defined-b) (values 7 8))
(test "define-values" 15 (+ defined-a defined-b))
(test "let-values parallel"
      3
      (let ((x 1))
        (let-values (((x y) (values 2 x)))
          (+ x y))))
(test "let*-values sequential"
      5
      (let*-values (((x y) (values 2 1))
                    ((z) (+ x y)))
        (+ z x)))
(define arity-proc
  (case-lambda
    (() 'zero)
    ((x) x)
    ((x y) (+ x y))
    ((a b c d e) (+ a b c d e))))
(test "case-lambda zero" 'zero (arity-proc))
(test "case-lambda one" 4 (arity-proc 4))
(test "case-lambda two" 9 (arity-proc 4 5))
(test "case-lambda five" 15 (arity-proc 1 2 3 4 5))
(define rest-arity-proc
  (case-lambda
    ((x y . rest) rest)
    (args args)))
(test "case-lambda dotted rest" '(3 4) (rest-arity-proc 1 2 3 4))
(test "case-lambda rest formals" '(1) (rest-arity-proc 1))

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
(test "take-right beyond length" '(1 2 3) (take-right '(1 2 3) 5))
(test "drop-right beyond length" '() (drop-right '(1 2 3) 5))
(test "split-at beyond length" '((1 2 3) ())
      (call-with-values
        (lambda () (split-at '(1 2 3) 5))
        list))
(test "split-at! beyond length" '((1 2 3) ())
      (let ((lst (list 1 2 3)))
        (call-with-values
          (lambda () (split-at! lst 5))
          list)))
(test "take! beyond length" '(1 2 3)
      (let ((lst (list 1 2 3)))
        (take! lst 5)))
(test "cut multiple slots" '(1 2 3 4 5)
      ((cut list 1 <> 3 <> 5) 2 4))
(test "cut rest slot" '(1 2 3 4 5)
      ((cut list 1 <> 3 <...>) 2 4 5))
(test "cute multiple slots" '(1 2 3 4 5)
      ((cute list 1 <> 3 <> 5) 2 4))
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
;;; Edge Cases and New Predicates
;;; ============================================================================

(section "Numeric Predicates (R5RS)")

; finite?, infinite?, nan? predicates
(test "finite? integer" #t (finite? 42))
(test "finite? float" #t (finite? 3.14159))
(test "finite? rational" #t (finite? 22/7))
(test "finite? bignum" #t (finite? 99999999999999999999))
(test "finite? infinity" #f (finite? (exp 1000)))
(test "infinite? integer" #f (infinite? 0))
(test "infinite? overflow" #t (infinite? (exp 1000)))
(test "nan? normal" #f (nan? 42))
(test "log -1 is complex" #t (complex? (log -1)))
(test "log -1 real-part" 0.0 (real-part (log -1)))

; complex number predicates
(test "finite? complex" #t (finite? (make-rectangular 3 4)))
(test "infinite? complex" #t (infinite? (make-rectangular (exp 1000) 0)))
(test "nan? complex" #f (nan? (make-rectangular 3 4)))

(section "String Edge Cases")

; Empty strings
(test "empty string length" 0 (string-length ""))
(test "empty string->list" '() (string->list ""))
(test "empty list->string" "" (list->string '()))
(test "empty string append" "hello" (string-append "" "hello" ""))
(test "empty substring" "" (substring "hello" 2 2))

; Long strings
(define long-str (make-string 1000 #\x))
(test "long string length" 1000 (string-length long-str))
(test "long string ref" #\x (string-ref long-str 999))

(section "Vector Edge Cases")

; Empty vectors
(test "empty vector length" 0 (vector-length '#()))
(test "empty vector->list" '() (vector->list '#()))
(test "empty list->vector" '#() (list->vector '()))

; Vector modification
(define v (make-vector 3 0))
(vector-set! v 0 'a)
(vector-set! v 2 'c)
(test "vector-set!" '#(a 0 c) v)

(section "Complex Numbers")

; Complex arithmetic
(test "complex magnitude" 5.0 (magnitude (make-rectangular 3 4)))
(test "complex real-part" 3 (real-part (make-rectangular 3 4)))
(test "complex imag-part" 4 (imag-part (make-rectangular 3 4)))
(test "real imag-part" 0 (imag-part 5))
(test "real real-part" 5 (real-part 5))

; Complex predicates
(test "complex? true" #t (complex? (make-rectangular 1 2)))
(test "complex? real" #t (complex? 5))  ; all reals are complex
(test "rational? int" #t (rational? 5))
(test "rational? frac" #t (rational? 3/4))

(section "Rational Numbers")

; numerator/denominator
(test "numerator int" 5 (numerator 5))
(test "denominator int" 1 (denominator 5))
(test "numerator frac" 3 (numerator 3/4))
(test "denominator frac" 4 (denominator 3/4))

(section "Bignum Rationals")

; Reading bignum rationals
(define big-rat 99999999999999999999/7)
(test "bignum rational read" 99999999999999999999/7 big-rat)
(test "bignum reduces to int" 33333333333333333333 99999999999999999999/3)
(test "bignum/bignum reduces" 9/8 99999999999999999999/88888888888888888888)
(test "bignum rational numerator" 1 (numerator 1/99999999999999999999))
(test "bignum rational denom" 99999999999999999999 (denominator 1/99999999999999999999))

; Arithmetic with bignum rationals
(test "bignum + rational" 199999999999999999999/2 (+ 99999999999999999999 1/2))
(test "bignum * rational" 99999999999999999999/2 (* 99999999999999999999 1/2))

(section "Exactness")

; exact/inexact conversions
(test "exact? int" #t (exact? 42))
(test "inexact? float" #t (inexact? 3.14))
(test "exact->inexact" 5.0 (exact->inexact 5))
(test "inexact->exact" 5 (inexact->exact 5.0))

;;; ============================================================================
;;; R7RS/MIT Scheme Compatibility Additions
;;; ============================================================================

(section "Compatibility Additions")

; Parameters
(define sample-parameter (make-parameter 10))
(test "parameter initial value" 10 (sample-parameter))
(sample-parameter 12)
(test "parameter set value" 12 (sample-parameter))
(test "parameterize dynamic value"
      '(99 12)
      (list (parameterize ((sample-parameter 99)) (sample-parameter))
            (sample-parameter)))

; cond-expand and feature inquiry
(test "features contains srfi-1" #t (if (memq 'srfi-1 (features)) #t #f))
(test "cond-expand selects feature"
      'has-bytevectors
      (cond-expand
        (bytevectors 'has-bytevectors)
        (else 'missing)))
(test "cond-expand supports and/not"
      'ok
      (cond-expand
        ((and vesper (not imaginary-feature)) 'ok)
        (else 'bad)))

; Hash tables
(define ht (make-hash-table))
(test "hash-table? true" #t (hash-table? ht))
(test "hash-table? false" #f (hash-table? '()))
(hash-table-set! ht "answer" 42)
(hash-table-set! ht '(a b) 'list-key)
(test "hash-table-ref string key" 42 (hash-table-ref ht "answer"))
(test "hash-table-ref equal list key" 'list-key (hash-table-ref ht '(a b)))
(test "hash-table-ref default" 'missing (hash-table-ref ht 'none 'missing))
(test "hash-table-exists?" #t (hash-table-exists? ht "answer"))
(test "hash-table-size" 2 (hash-table-size ht))
(hash-table-delete! ht "answer")
(test "hash-table-delete!" #f (hash-table-exists? ht "answer"))
(define resize-ht (make-hash-table 1))
(let loop ((i 0))
  (if (< i 100)
      (begin
        (hash-table-set! resize-ht i (* i i))
        (loop (+ i 1)))))
(test "hash-table resizes" 9801 (hash-table-ref resize-ht 99))
(test "hash-table size after resize" 100 (hash-table-size resize-ht))
(define equal-ht (make-equal-hash-table))
(hash-table-set! equal-ht '(x y) 17)
(test "make-equal-hash-table uses equal?" 17 (hash-table-ref equal-ht '(x y)))
(define eq-key (list 'x 'y))
(define eq-ht (make-strong-eq-hash-table))
(hash-table-set! eq-ht eq-key 23)
(test "make-strong-eq-hash-table same object" 23 (hash-table-ref eq-ht eq-key))
(test "make-strong-eq-hash-table not equal list" 'missing
      (hash-table-ref eq-ht '(x y) 'missing))
(define eqv-ht (make-strong-eqv-hash-table))
(hash-table-set! eqv-ht 1000 'numeric)
(hash-table-set! eqv-ht "key" 'string-object)
(test "make-strong-eqv-hash-table numeric" 'numeric (hash-table-ref eqv-ht 1000))
(test "make-strong-eqv-hash-table not string equal" 'missing
      (hash-table-ref eqv-ht (string-append "k" "ey") 'missing))
(test "hash-table-keys" #t (if (member '(a b) (hash-table-keys ht)) #t #f))
(test "hash-table-values" #t (if (memq 'list-key (hash-table-values ht)) #t #f))
(test "hash-table->alist" #t (if (assoc '(a b) (hash-table->alist ht)) #t #f))
(hash-table-update! ht '(a b) (lambda (x) (list x 'updated)))
(test "hash-table-update!" '(list-key updated) (hash-table-ref ht '(a b)))
(test "hash-table-ref/default" 'fallback
      (hash-table-ref/default ht 'absent 'fallback))
(define walk-sum 0)
(hash-table-walk resize-ht (lambda (k v) (set! walk-sum (+ walk-sum 1))))
(test "hash-table-walk" 100 walk-sum)
(test "hash-table-fold" 100
      (hash-table-fold (lambda (k v acc) (+ acc 1)) 0 resize-ht))
(test "hash-table-map" #t
      (if (member 9801 (hash-table-map (lambda (k v) v) resize-ht)) #t #f))
(define resize-ht-copy (hash-table-copy resize-ht))
(test "hash-table-copy size" 100 (hash-table-size resize-ht-copy))
(test "hash-table-copy value" 9801 (hash-table-ref resize-ht-copy 99))
(hash-table-clear! resize-ht)
(test "hash-table-clear!" 0 (hash-table-size resize-ht))

; I/O helpers and port predicates
(define out-str-port (open-output-string))
(write-string "abc" out-str-port)
(test "write-string" "abc" (get-output-string out-str-port))
(define out-str-port2 (open-output-string))
(write-string "abcdef" out-str-port2 2 5)
(test "write-string range" "cde" (get-output-string out-str-port2))
(define in-str-port (open-input-string "abcdef"))
(test "read-string" "abc" (read-string 3 in-str-port))
(test "input-port-open?" #t (input-port-open? in-str-port))
(close-input-port in-str-port)
(test "input-port-open? closed" #f (input-port-open? in-str-port))
(define peek-u8-port (open-input-string "AZ"))
(test "peek-u8" (char->integer #\A) (peek-u8 peek-u8-port))
(test "peek-u8 does not consume" (char->integer #\A) (read-u8 peek-u8-port))
(test "read-u8 after peek" (char->integer #\Z) (read-u8 peek-u8-port))
(test "peek-u8 eof" #t (eof-object? (peek-u8 peek-u8-port)))
(close-input-port peek-u8-port)
(test "current-error-port output" #t (output-port? (current-error-port)))
(test "string port textual" #t (textual-port? (open-input-string "abc")))

; File/OS helpers
(define compat-file "/tmp/vesper-compat-test.bin")
(define compat-out (open-binary-output-file compat-file))
(test "binary output port predicate" #t (binary-port? compat-out))
(test "binary output not textual" #f (textual-port? compat-out))
(write-bytevector (bytevector 65 66 67) compat-out)
(close-output-port compat-out)
(test "file-exists? compat file" #t (file-exists? compat-file))
(define compat-in (open-binary-input-file compat-file))
(test "binary input port predicate" #t (binary-port? compat-in))
(test "read-bytevector file" #u8(65 66 67) (read-bytevector 3 compat-in))
(close-input-port compat-in)
(delete-file compat-file)
(test "delete-file compat file" #f (file-exists? compat-file))
(define compat-text-file "/tmp/vesper-compat-text.txt")
(define compat-text-out (open-output-file compat-text-file))
(test "text output port predicate" #t (textual-port? compat-text-out))
(test "text output not binary" #f (binary-port? compat-text-out))
(close-output-port compat-text-out)
(delete-file compat-text-file)
(test "current-directory returns string" #t (string? (current-directory)))
(test "directory-files returns list" #t (list? (directory-files ".")))
(test "current-jiffy integer" #t (integer? (current-jiffy)))
(test "jiffies-per-second" 1000000000 (jiffies-per-second))
(test "get-environment-variables returns alist"
      #t
      (let ((envs (get-environment-variables)))
        (or (null? envs)
            (and (pair? (car envs))
                 (string? (caar envs))
                 (string? (cdar envs))))))

(section "Additional R7RS Compatibility")

(test "boolean=? true" #t (boolean=? #t #t #t))
(test "boolean=? false" #f (boolean=? #t #t #f))
(test "boolean=? rejects non-boolean" #f (boolean=? #t 1))
(define call-with-port-result
  (let ((p (open-input-string "x")))
    (cons (call-with-port p read-char)
          (input-port-open? p))))
(test "call-with-port closes" (cons #\x #f) call-with-port-result)
(define close-port-output (open-output-string))
(test "port? output" #t (port? close-port-output))
(close-port close-port-output)
(test "close-port closes output" #f (output-port-open? close-port-output))
(test "exact alias" #t (exact? (exact 1.0)))
(test "inexact alias" #t (inexact? (inexact 1)))
(test "exact-integer?" #t (exact-integer? 42))
(test "exact-integer? inexact" #f (exact-integer? 42.0))
(test "floor-quotient positive" 3 (floor-quotient 10 3))
(test "floor-remainder positive" 1 (floor-remainder 10 3))
(test "floor-quotient negative" -4 (floor-quotient -10 3))
(test "floor-remainder negative" 2 (floor-remainder -10 3))
(test "floor/ values" '(-4 2)
      (call-with-values (lambda () (floor/ -10 3)) list))
(test "truncate-quotient negative" -3 (truncate-quotient -10 3))
(test "truncate-remainder negative" -1 (truncate-remainder -10 3))
(test "truncate/ values" '(-3 -1)
      (call-with-values (lambda () (truncate/ -10 3)) list))
(test "truncate/ bignum values"
      '(-33333333333333333333 -1)
      (call-with-values
        (lambda () (truncate/ -100000000000000000000 3))
        list))
(test "floor/ bignum values"
      '(-33333333333333333334 2)
      (call-with-values
        (lambda () (floor/ -100000000000000000000 3))
        list))
(test "letrec*" 3
      (letrec* ((x 1)
                (y (+ x 2)))
        y))
(define list-set-target (list 'a 'b 'c))
(list-set! list-set-target 1 'x)
(test "list-set!" '(a x c) list-set-target)
(test "square" 49 (square 7))
(test "string->vector" '#(#\b #\c) (string->vector "abcd" 1 3))
(test "symbol=? true" #t (symbol=? 'a 'a 'a))
(test "symbol=? false" #f (symbol=? 'a 'a 'b))
(test "symbol=? rejects non-symbol" #f (symbol=? 'a 1))
(test "vector->string" "bc" (vector->string '#(#\a #\b #\c #\d) 1 3))
(test "write-simple" "\"x\"" (call-with-output-string
                                (lambda (p) (write-simple "x" p))))
(test "write-shared" "(1 2)" (call-with-output-string
                                (lambda (p) (write-shared '(1 2) p))))
(test "exact-nonnegative-integer?" #t (exact-nonnegative-integer? 0))
(test "exact-rational?" #t (exact-rational? 1/3))
(test "exact-integer-sqrt" '(5 2)
      (call-with-values (lambda () (exact-integer-sqrt 27)) list))
(test "euclidean/" '(4 3)
      (call-with-values (lambda () (euclidean/ -13 -4)) list))
(test "ceiling/" '(4 -3)
      (call-with-values (lambda () (ceiling/ 13 4)) list))
(test "round/" '(4 -1)
      (call-with-values (lambda () (round/ 15 4)) list))
(test "integer-divide" '(-3 -1)
      (let ((qr (integer-divide -10 3)))
        (list (integer-divide-quotient qr)
              (integer-divide-remainder qr))))
(test "modexp" 46 (modexp 1234 5678 90))
(test "floor->exact" #t (exact? (floor->exact 3.7)))
(test "logistic" 0.5 (logistic 0))
(test "conjugate" -2 (imag-part (conjugate (make-rectangular 1 2))))
(test "stable log1p tiny" #t (< (abs (- (log1p 1e-20) 1e-20)) 1e-30))
(test "stable expm1 tiny" #t (< (abs (- (expm1 1e-20) 1e-20)) 1e-30))
(test "stable sqrt1pm1 tiny" #t (< (abs (- (sqrt1pm1 1e-20) 5e-21)) 1e-30))
(test "stable log1pexp large" 1000.0 (log1pexp 1000.0))
(test "complex stable numerical helpers" #t
      (and (complex? (log1p 1+2i))
           (complex? (expm1 1+2i))
           (complex? (sqrt1pm1 1+2i))
           (complex? (log1pexp 1+2i))))
(test "log1p real branch cut" #t
      (and (complex? (log1p -2))
           (= (real-part (log1p -2)) 0.0)))
(test "sqrt1pm1 real branch cut" #t
      (and (complex? (sqrt1pm1 -2))
           (= (real-part (sqrt1pm1 -2)) -1.0)
           (= (imag-part (sqrt1pm1 -2)) 1.0)))
(test "log1pexp large complex finite" #t
      (let ((z (log1pexp 1000+1i)))
        (and (finite? (real-part z))
             (finite? (imag-part z))
             (< (abs (- (real-part z) 1000.0)) 1e-12)
             (< (abs (- (imag-part z) 1.0)) 1e-12))))
(test "digit-value" 15 (digit-value #\f))
(test "unicode digit-value" 5 (digit-value (integer->char #x0665)))
(test "char-foldcase" #\a (char-foldcase #\A))
(test "string-foldcase" "abc" (string-foldcase "AbC"))
(test "unicode char predicates" #t
      (and (char-alphabetic? (integer->char #x03bb))
           (char-upper-case? (integer->char #x0391))
           (char-lower-case? (integer->char #x03b1))
           (char-whitespace? (integer->char #x2003))))
(test "unicode char-foldcase final sigma" (integer->char #x03c3)
      (char-foldcase (integer->char #x03c2)))
(test "unicode string-upcase expands" "SSFFI" (string-upcase "ßﬃ"))
(test "unicode string-downcase final sigma"
      (unicode-char #x03bf #x03c2)
      (string-downcase (unicode-char #x039f #x03a3)))
(test "unicode string-titlecase final sigma"
      (unicode-char #x039f #x03c2)
      (string-titlecase (unicode-char #x039f #x03a3)))
(test "unicode string-foldcase expands" "ssi̇ksμσσσffi"
      (string-foldcase "ẞİKſµΣςσﬃ"))
(test "unicode string-ci uses full foldcase" #t
      (and (string-ci=? "straße" "STRASSE")
           (string-ci=? (unicode-char #x0399 #x03A0 #x03A0 #x039F #x03A3)
                        (unicode-char #x03B9 #x03C0 #x03C0 #x03BF #x03C3))))
(test "unicode normalize nfd decomposes latin accent"
      (unicode-char 101 769)
      (string-normalize-nfd (unicode-char 233)))
(test "unicode normalize nfc composes latin accent"
      (unicode-char 233)
      (string-normalize-nfc (unicode-char 101 769)))
(test "unicode normalize orders combining marks"
      (unicode-char 97 807 769)
      (string-normalize-nfd (unicode-char 97 769 807)))
(test "unicode normalize nfkd expands ligature"
      "ffi"
      (string-normalize-nfkd (string (integer->char 64259))))
(test "unicode normalize nfkc fullwidth"
      "ABC123"
      (string-normalize-nfkc (unicode-char 65313 65314 65315 65297 65298 65299)))
(test "unicode normalized nfc predicate" #t
      (and (string-normalized-nfc? (unicode-char 233))
           (not (string-normalized-nfc? (unicode-char 101 769)))))
(test "features contains unicode tables" #t
      (and (not (not (memq 'unicode-normalization (features))))
           (not (not (memq 'unicode-case-folding (features))))
           (not (not (memq 'unicode-character-properties (features))))))
(test "utf8 roundtrip" "abc" (utf8->string (string->utf8 "abc")))
(test "utf8 encodes two-byte character" #u8(195 169)
      (string->utf8 (string (integer->char 233))))
(test "utf8 decodes two-byte character" (string (integer->char 233))
      (utf8->string #u8(195 169)))
(test "environment returns environment" #t (pair? (environment '(scheme base))))
(test "environment rejects unknown library"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (environment '(scheme imaginary))))
(define-library (compat smoke)
  (export define-library-value)
  (import (scheme base))
  (begin
    (define define-library-value 91)))
(test "define-library begin" 91 define-library-value)
(test "error object guard"
      '("boom" (1 2))
      (guard (exn
              ((error-object? exn)
               (list (error-object-message exn)
                     (error-object-irritants exn))))
        (error "boom" 1 2)))
(test "syntax-error object kind"
      #t
      (guard (exn
              ((read-error? exn) #t)
              (else #f))
        (syntax-error "bad syntax")))
(test "file-error? structured object"
      #t
      (file-error? (make-error-object 'file "missing" '("x"))))
(define include-test-file "/tmp/vesper-include-test.scm")
(define include-test-out (open-output-file include-test-file))
(write-string "(define included-value 77)" include-test-out)
(close-output-port include-test-out)
(include "/tmp/vesper-include-test.scm")
(test "include" 77 included-value)
(delete-file include-test-file)
(define-record-type record-collision
  (make-record-a x)
  record-a?
  (x record-a-x set-record-a-x!))
(define-record-type record-collision
  (make-record-b x)
  record-b?
  (x record-b-x))
(define-record-type reordered-record
  (make-reordered y x)
  reordered-record?
  (x reordered-x)
  (y reordered-y))
(define-record-type partial-record
  (make-partial y)
  partial-record?
  (x partial-x)
  (y partial-y))
(define record-a-instance (make-record-a 1))
(define record-b-instance (make-record-b 2))
(define reordered-record-instance (make-reordered 20 10))
(define partial-record-instance (make-partial 30))
(test "record unique tag" #f (record-a? record-b-instance))
(test "record accessor" 1 (record-a-x record-a-instance))
(set-record-a-x! record-a-instance 3)
(test "record mutator" 3 (record-a-x record-a-instance))
(test "record constructor reordered fields" '(10 20)
      (list (reordered-x reordered-record-instance)
            (reordered-y reordered-record-instance)))
(test "record constructor omitted field" #f
      (partial-x partial-record-instance))
(test "record constructor included field" 30
      (partial-y partial-record-instance))
(test "record accessor rejects wrong type"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (record-a-x record-b-instance)))
(test "record rejects duplicate fields"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x)
            bad-record?
            (x bad-x)
            (x bad-x2))
          #f)))
(test "record rejects duplicate constructor fields"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x x)
            bad-record?
            (x bad-x))
          #f)))
(test "record rejects unknown constructor field"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record y)
            bad-record?
            (x bad-x))
          #f)))
(test "record rejects duplicate generated binding"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x)
            bad-record?
            (x make-bad-record))
          #f)))

(section "Vector/String Additions")

(test "vector-copy" '#(2 3) (vector-copy '#(1 2 3 4) 1 3))
(define copy-target (vector 0 0 0 0))
(vector-copy! copy-target 1 '#(8 9))
(test "vector-copy!" '#(0 8 9 0) copy-target)
(test "vector-append" '#(1 2 3 4) (vector-append '#(1 2) '#(3 4)))
(test "vector-map" '#(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6)))
(define vector-for-each-sum 0)
(vector-for-each (lambda (x) (set! vector-for-each-sum (+ vector-for-each-sum x)))
                 '#(1 2 3))
(test "vector-for-each" 6 vector-for-each-sum)
(test "string-map" "bcd"
      (string-map (lambda (ch) (integer->char (+ 1 (char->integer ch)))) "abc"))
(define string-for-each-sum 0)
(string-for-each
  (lambda (ch)
    (set! string-for-each-sum (+ string-for-each-sum (char->integer ch))))
  "ab")
(test "string-for-each" (+ (char->integer #\a) (char->integer #\b))
      string-for-each-sum)
(define string-copy-target (string #\x #\x #\x #\x))
(string-copy! string-copy-target 1 "abcd" 1 3)
(test "string-copy!" "xbcx" string-copy-target)
(define unicode-string "aé𝄞")
(test "utf8 string-length counts characters" 3 (string-length unicode-string))
(test "utf8 string->list decodes characters" '(97 233 119070)
      (map char->integer (string->list unicode-string)))
(test "utf8 string-ref uses character index" 119070
      (char->integer (string-ref unicode-string 2)))
(test "utf8 substring uses character indexes" "é𝄞"
      (substring unicode-string 1 3))
(test "utf8 string-set! can change byte width"
      "a𝄞𝄞"
      (let ((s (string-copy unicode-string)))
        (string-set! s 1 (integer->char 119070))
        s))
(test "utf8 string-fill! range"
      "aéé"
      (let ((s (string-copy unicode-string)))
        (string-fill! s (integer->char 233) 1 3)
        s))
(test "utf8 escape string literal" 119070
      (char->integer (string-ref "\x1D11E;" 0)))
(test "unicode character hex literal" 119070
      (char->integer #\x1D11E))
(test "unicode character utf8 literal" 955
      (char->integer #\λ))
(test "unicode write char roundtrip" "#\\x1D11E"
      (write-to-string (integer->char #x1D11E)))
(test "unicode read written char roundtrip" 119070
      (char->integer (read (open-input-string
                            (write-to-string (integer->char #x1D11E))))))
(test "unicode identifier preserves utf8" 42
      (let ((λ 42)) λ))
(test "unicode identifier ascii-folds only ascii" 7
      (let ((fooλ 7)) Fooλ))
(test "reader no-fold-case directive" "FOO"
      (symbol->string (read (open-input-string "#!no-fold-case FOO"))))
(test "reader fold-case directive" "foo"
      (symbol->string (read (open-input-string "#!fold-case FOO"))))
(test "reader case directive toggles" '("FOO" "bar")
      (let ((p (open-input-string "#!no-fold-case FOO #!fold-case BAR")))
        (list (symbol->string (read p)) (symbol->string (read p)))))
(test "escaped identifier preserves case and spaces" 9
      (let ((|Hello World| 9)) |Hello World|))
(test "escaped identifier scalar escape" 11
      (let ((|\x3BB;| 11)) λ))
(test "write escaped symbol roundtrip" "Hello World"
      (symbol->string (read (open-input-string
                             (write-to-string
                              (string->symbol "Hello World"))))))
(test "string-append*" "abc" (string-append* '("a" "b" "c")))
(test "string*" "a12#t" (string* (list "a" 12 #t)))
(test "string-compare" 'lt
      (string-compare "a" "b" (lambda () 'eq) (lambda () 'lt) (lambda () 'gt)))
(test "string-upper-case?" #t (string-upper-case? "ABC"))
(test "string-lower-case?" #f (string-lower-case? "Abc"))
(test "string-count" 3 (string-count char-alphabetic? "abc123"))
(test "string-any" #\b (string-any (lambda (c) (and (char=? c #\b) c)) "abc"))
(test "string-every" #t (string-every char-alphabetic? "abc"))
(test "string-null?" #t (string-null? ""))
(test "string-head" "ab" (string-head "abcd" 2))
(test "string-tail" "cd" (string-tail "abcd" 2))
(test "string-hash modulus" #t (< (string-hash "abc" 10) 10))
(define builder (string-builder))
(builder #\a)
(builder "bé")
(test "string-builder count" 3 (builder 'count))
(test "string-builder value" "abé" (builder))
(builder 'reset!)
(test "string-builder empty" #t (builder 'empty?))
(test "string-join default separator" "a b" (string-join '("a" "b")))
(test "string-join explicit separator" "a,b" (string-join '("a" "b") ","))
(test "string-join prefix suffix" "[a,b]" (string-join '("a" "b") "," "[" "]"))
(test "string-joiner" "<a|b>" ((string-joiner 'infix "|" 'prefix "<" 'suffix ">") "a" "b"))
(test "string-joiner*" "<a|b>" ((string-joiner* 'infix "|" 'prefix "<" 'suffix ">") '("a" "b")))
(test "string-splitter runs" '("a" "b") ((string-splitter 'delimiter #\,) "a,,b"))
(test "string-splitter no runs" '("a" "" "b")
      ((string-splitter 'delimiter #\, 'allow-runs? #f) "a,,b"))
(test "string-pad-left" "..abc" (string-pad-left "abc" 5 #\.))
(test "string-pad-right" "abc.." (string-pad-right "abc" 5 #\.))
(test "string-trimmer" "abc" ((string-trimmer) "  abc  "))
(test "string-replace" "bonono" (string-replace "banana" #\a #\o))
(test "string-slice" "é𝄞" (string-slice unicode-string 1 3))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(if (= fail-count 0)
    (display "All tests passed!")
    (display "SOME TESTS FAILED!"))
(newline)
