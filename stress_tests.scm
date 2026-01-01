;;; Stress Tests for Scheme Interpreter
;;; Tests extreme cases for both CPS and bytecode modes
;;; Run with: ./lisp stress_tests.scm (bytecode)
;;;           ./lisp --interpreter stress_tests.scm (CPS)

(define *test-pass* 0)
(define *test-fail* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! *test-pass* (+ *test-pass* 1))
        (display "  PASS: ") (display name) (newline))
      (begin
        (set! *test-fail* (+ *test-fail* 1))
        (display "  FAIL: ") (display name)
        (display " - expected ") (write expected)
        (display " got ") (write actual) (newline))))

(define (section name)
  (newline)
  (display "=== ") (display name) (display " ===") (newline))

;; ============================================================================
;; Deep Recursion Stress
;; ============================================================================

(section "Deep Recursion")

;; Deep tail recursion - should not stack overflow
(define (deep-tail n acc)
  (if (= n 0) acc (deep-tail (- n 1) (+ acc 1))))

(test "tail recursion 100000" 100000 (deep-tail 100000 0))

;; Mutual recursion
(define (even-rec? n)
  (if (= n 0) #t (odd-rec? (- n 1))))
(define (odd-rec? n)
  (if (= n 0) #f (even-rec? (- n 1))))

(test "mutual recursion even 10000" #t (even-rec? 10000))
(test "mutual recursion odd 10001" #t (odd-rec? 10001))

;; Non-tail recursion (limited depth)
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(test "fib(20) non-tail" 6765 (fib 20))

;; Deep let nesting via loop
(define (nested-lets depth)
  (let loop ((d depth) (acc 0))
    (if (= d 0)
        acc
        (let ((x d))
          (loop (- d 1) (+ acc x))))))

(test "nested lets 5000" 12502500 (nested-lets 5000))

;; ============================================================================
;; Heavy Allocation Stress (GC Pressure)
;; ============================================================================

(section "Heavy Allocation / GC Stress")

;; Allocate many small lists
(define (alloc-small-lists n)
  (let loop ((i n) (acc '()))
    (if (= i 0)
        (length acc)
        (loop (- i 1) (cons (list i (+ i 1) (+ i 2)) acc)))))

(test "allocate 10000 small lists" 10000 (alloc-small-lists 10000))

;; Allocate and immediately discard (pure GC stress)
(define (alloc-discard n)
  (let loop ((i n))
    (if (= i 0)
        'done
        (begin
          (make-list 100)  ; Allocate and discard
          (loop (- i 1))))))

(test "allocate/discard 5000 lists" 'done (alloc-discard 5000))

;; Create deep tree structure
(define (make-tree depth)
  (if (= depth 0)
      'leaf
      (cons (make-tree (- depth 1))
            (make-tree (- depth 1)))))

(define (count-leaves tree)
  (if (eq? tree 'leaf)
      1
      (+ (count-leaves (car tree))
         (count-leaves (cdr tree)))))

(test "binary tree depth 15" 32768 (count-leaves (make-tree 15)))

;; Vector allocation stress
(define (vector-alloc-stress n)
  (let loop ((i n) (acc 0))
    (if (= i 0)
        acc
        (let ((v (make-vector 50 i)))
          (loop (- i 1) (+ acc (vector-ref v 25)))))))

(test "vector allocation 2000" 2001000 (vector-alloc-stress 2000))

;; String allocation stress
(define (string-alloc-stress n)
  (let loop ((i n) (acc ""))
    (if (= i 0)
        (string-length acc)
        (loop (- i 1) (string-append acc "x")))))

(test "string concatenation 1000" 1000 (string-alloc-stress 1000))

;; ============================================================================
;; Continuation Stress Tests
;; ============================================================================

(section "Continuation Stress")

;; Deep continuation capture
(define (deep-callcc depth)
  (if (= depth 0)
      (call/cc (lambda (k) (k 0)))
      (+ 1 (deep-callcc (- depth 1)))))

(test "deep call/cc 100" 100 (deep-callcc 100))

;; Actually test: call/cc captures and escapes from depth
(define (deep-escape depth)
  (call/cc (lambda (exit)
    (let loop ((d depth))
      (if (= d 0)
          (exit 'escaped)
          (+ 1 (loop (- d 1))))))))

(test "escape from depth 500" 'escaped (deep-escape 500))

;; Multi-shot continuation stress
(define (multi-shot-stress n)
  (let ((k #f)
        (count 0))
    (+ (call/cc (lambda (cont)
                  (set! k cont)
                  0))
       0)
    (set! count (+ count 1))
    (if (< count n)
        (k 0)
        count)))

(test "multi-shot 100 times" 100 (multi-shot-stress 100))

;; Continuation with allocation
(define (cont-with-alloc n)
  (let ((k #f)
        (count 0)
        (data '()))
    (call/cc (lambda (cont)
               (set! k cont)
               0))
    (set! count (+ count 1))
    (set! data (cons (make-list 10) data))  ; Allocate on each restart
    (if (< count n)
        (k 0)
        (length data))))

(test "continuation with allocation 50" 50 (cont-with-alloc 50))

;; Nested continuations
(define (nested-cont depth)
  (if (= depth 0)
      0
      (+ 1 (call/cc (lambda (k)
                      (k (nested-cont (- depth 1))))))))

(test "nested continuations 50" 50 (nested-cont 50))

;; ============================================================================
;; Closure Stress Tests
;; ============================================================================

(section "Closure Stress")

;; Many closures sharing environment
(define (make-counter-family n)
  (let ((shared 0))
    (let loop ((i n) (counters '()))
      (if (= i 0)
          counters
          (loop (- i 1)
                (cons (lambda ()
                        (set! shared (+ shared 1))
                        shared)
                      counters))))))

(define counter-family (make-counter-family 100))
(define (call-all-counters counters)
  (map (lambda (c) (c)) counters))

(test "shared closure counter"
      100
      (car (reverse (call-all-counters counter-family))))

;; Deep closure chain
(define (deep-closure-chain depth)
  (if (= depth 0)
      (lambda () 0)
      (let ((inner (deep-closure-chain (- depth 1))))
        (lambda () (+ 1 (inner))))))

(test "closure chain depth 100" 100 ((deep-closure-chain 100)))

;; Closures capturing different depths
(define (capture-at-depths n)
  (let loop ((i n) (closures '()))
    (if (= i 0)
        closures
        (let ((captured i))
          (loop (- i 1)
                (cons (lambda () captured) closures))))))

(define depth-closures (capture-at-depths 100))
(test "closures at different depths"
      5050
      (apply + (map (lambda (f) (f)) depth-closures)))

;; ============================================================================
;; Higher-Order Function Stress
;; ============================================================================

(section "Higher-Order Functions")

;; Deep map chains
(define (chain-maps n lst)
  (if (= n 0)
      lst
      (chain-maps (- n 1) (map (lambda (x) (+ x 1)) lst))))

(test "chain 100 maps"
      (map (lambda (x) (+ x 100)) '(1 2 3 4 5))
      (chain-maps 100 '(1 2 3 4 5)))

;; Nested filter/map
(define (nested-filter-map n lst)
  (if (= n 0)
      lst
      (nested-filter-map (- n 1)
                         (filter (lambda (x) (> x 0))
                                 (map (lambda (x) (- x 1)) lst)))))

(define start-list (iota 50))
(test "nested filter/map terminates" '() (nested-filter-map 100 start-list))

;; Fold stress
(define (fold-stress n)
  (fold + 0 (iota n)))

(test "fold 10000 elements" 49995000 (fold-stress 10000))

;; Compose many functions
(define (compose-n n f)
  (if (= n 0)
      (lambda (x) x)
      (let ((inner (compose-n (- n 1) f)))
        (lambda (x) (f (inner x))))))

(define add1-100 (compose-n 100 (lambda (x) (+ x 1))))
(test "compose 100 functions" 100 (add1-100 0))

;; ============================================================================
;; Numeric Tower Stress
;; ============================================================================

(section "Numeric Tower")

;; Large integer arithmetic
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define fact50 (factorial 50))
(test "factorial 50 is positive" #t (> fact50 0))
(test "factorial 50 is exact" #t (exact? fact50))

;; Rational arithmetic stress
(define (harmonic-sum n)
  (let loop ((i 1) (sum 0))
    (if (> i n)
        sum
        (loop (+ i 1) (+ sum (/ 1 i))))))

(define h20 (harmonic-sum 20))
(test "harmonic sum 20 is rational" #t (rational? h20))

;; Mixed numeric operations
(define (mixed-numeric n)
  (let loop ((i n) (acc 0))
    (if (= i 0)
        acc
        (loop (- i 1)
              (+ acc
                 (/ i (+ i 1))
                 (* 0.5 i)
                 (sqrt i))))))

(test "mixed numeric 100" #t (> (mixed-numeric 100) 0))

;; Complex number stress
(define (complex-spiral n)
  (let loop ((i n) (z 0+0i))
    (if (= i 0)
        z
        (loop (- i 1) (+ z (make-rectangular (cos i) (sin i)))))))

(test "complex spiral 100" #t (complex? (complex-spiral 100)))

;; ============================================================================
;; Macro Stress Tests
;; ============================================================================

(section "Macro Stress")

;; Deeply nested macro expansion
(define-syntax nest-add
  (syntax-rules ()
    ((_ e) e)
    ((_ e1 e2 ...) (+ e1 (nest-add e2 ...)))))

(test "nested macro 20 deep"
      190
      (nest-add 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

;; Macro generating many bindings
(define-syntax multi-let
  (syntax-rules ()
    ((_ () body) body)
    ((_ (v vs ...) body)
     (let ((v 1)) (multi-let (vs ...) body)))))

(test "macro multi-let" 10 (multi-let (a b c d e f g h i j) (+ a b c d e f g h i j)))

;; Recursive macro stress
(define-syntax sum-to-macro
  (syntax-rules ()
    ((_ 0) 0)
    ((_ 1) 1)
    ((_ 2) 3)
    ((_ 3) 6)
    ((_ 4) 10)
    ((_ 5) 15)))

(test "recursive-like macro" 15 (sum-to-macro 5))

;; ============================================================================
;; Dynamic-wind Stress
;; ============================================================================

(section "Dynamic-wind Stress")

;; Nested dynamic-wind
(define (nested-dw depth)
  (define count 0)
  (define (go d)
    (if (= d 0)
        'done
        (dynamic-wind
          (lambda () (set! count (+ count 1)))
          (lambda () (go (- d 1)))
          (lambda () (set! count (+ count 1))))))
  (go depth)
  count)

(test "nested dynamic-wind 50" 100 (nested-dw 50))

;; Dynamic-wind with continuation jumps
(define (dw-cont-stress n)
  (let ((before-count 0)
        (after-count 0)
        (k #f)
        (i 0))
    (dynamic-wind
      (lambda () (set! before-count (+ before-count 1)))
      (lambda ()
        (call/cc (lambda (cont) (set! k cont)))
        (set! i (+ i 1)))
      (lambda () (set! after-count (+ after-count 1))))
    (if (< i n)
        (k 'again)
        (list before-count after-count))))

(test "dw with cont stress 20" '(20 20) (dw-cont-stress 20))

;; ============================================================================
;; Values/Multiple-Values Stress
;; ============================================================================

(section "Multiple Values Stress")

;; Pass many values
(define (many-values n)
  (apply values (iota n)))

(test "receive 50 values"
      1225
      (call-with-values
        (lambda () (many-values 50))
        +))

;; Chain values through multiple call-with-values
(define (chain-values n start)
  (if (= n 0)
      start
      (call-with-values
        (lambda () (values (+ start 1) (+ start 2)))
        (lambda (a b) (chain-values (- n 1) (+ a b))))))

;; Formula: start_n = 3 * 2^n - 3, so for n=50: 3377699720527869
(test "chain values 50" 3377699720527869 (chain-values 50 0))

;; ============================================================================
;; Apply Stress
;; ============================================================================

(section "Apply Stress")

;; Apply with long argument list
(test "apply + 1000 args" 499500 (apply + (iota 1000)))

;; Nested apply
(define (nested-apply n)
  (if (= n 0)
      (list 1 2 3)
      (apply list (nested-apply (- n 1)))))

(test "nested apply 20" '(1 2 3) (nested-apply 20))

;; ============================================================================
;; Eval Stress (if available)
;; ============================================================================

(section "Eval Stress")

;; Build and evaluate expressions
(define (make-nested-add depth)
  (if (= depth 0)
      1
      (list '+ 1 (make-nested-add (- depth 1)))))

;; Use 2-arg form for CPS mode compatibility
(test "eval nested add 50" 51 (eval (make-nested-add 50) (interaction-environment)))

;; ============================================================================
;; Combined Stress Tests
;; ============================================================================

(section "Combined Stress")

;; GC + Continuations + Closures
(define (combined-stress n)
  (let ((k #f)
        (closures '())
        (count 0))
    (call/cc (lambda (cont) (set! k cont)))
    (set! closures (cons (let ((x count))
                           (lambda () x))
                         closures))
    (set! count (+ count 1))
    ;; Force some allocation
    (make-list 100)
    (if (< count n)
        (k 'again)
        (apply + (map (lambda (f) (f)) closures)))))

(test "combined stress 50" 1225 (combined-stress 50))

;; Amb-style backtracking with heavy allocation
(define (search-with-alloc target)
  (call/cc (lambda (found)
    (let loop ((i 0))
      (if (> i 1000)
          #f
          (begin
            (make-list 50)  ; Allocate
            (if (= i target)
                (found i)
                (loop (+ i 1)))))))))

(test "search with allocation" 500 (search-with-alloc 500))

;; Deep recursion with continuation escape
(define (deep-with-escape depth target)
  (call/cc (lambda (exit)
    (let loop ((d depth) (acc 0))
      (if (= d 0)
          acc
          (begin
            (if (= acc target) (exit 'found))
            (loop (- d 1) (+ acc 1))))))))

(test "deep recursion with escape" 'found (deep-with-escape 10000 500))

;; ============================================================================
;; Summary
;; ============================================================================

(newline)
(display "========================================") (newline)
(display "Stress Tests: ")
(display (+ *test-pass* *test-fail*))
(display ", Passed: ")
(display *test-pass*)
(display ", Failed: ")
(display *test-fail*)
(newline)
(if (= *test-fail* 0)
    (display "All stress tests passed!\n")
    (begin
      (display "SOME TESTS FAILED!\n")
      (exit 1)))
