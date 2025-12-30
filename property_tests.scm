;;; Property-Based Tests for Scheme Interpreter
;;; Uses random generation to test invariants

;; Configuration
(define *num-tests* 100)
(define *max-list-size* 50)
(define *max-int* 10000)

;; Test framework
(define *passed* 0)
(define *failed* 0)
(define *total-checks* 0)

(define (check name property-fn generator-fn)
  (let loop ((i 0) (failed-inputs '()))
    (if (>= i *num-tests*)
        (if (null? failed-inputs)
            (begin
              (set! *passed* (+ *passed* 1))
              (display "  PASS: ") (display name) 
              (display " (") (display *num-tests*) (display " tests)")
              (newline))
            (begin
              (set! *failed* (+ *failed* 1))
              (display "  FAIL: ") (display name)
              (display " - failed on: ") (display (car failed-inputs))
              (newline)))
        (let* ((input (generator-fn))
               (result (property-fn input)))
          (set! *total-checks* (+ *total-checks* 1))
          (if result
              (loop (+ i 1) failed-inputs)
              (loop (+ i 1) (cons input failed-inputs)))))))

;; Generators
(define (gen-int)
  (- (random-integer (* 2 *max-int*)) *max-int*))

(define (gen-nat)
  (random-integer *max-int*))

(define (gen-positive)
  (+ 1 (random-integer (- *max-int* 1))))

(define (gen-list gen-elem)
  (lambda ()
    (let ((len (random-integer *max-list-size*)))
      (let loop ((i 0) (acc '()))
        (if (>= i len)
            acc
            (loop (+ i 1) (cons (gen-elem) acc)))))))

(define (gen-nonempty-list gen-elem)
  (lambda ()
    (let ((len (+ 1 (random-integer (- *max-list-size* 1)))))
      (let loop ((i 0) (acc '()))
        (if (>= i len)
            acc
            (loop (+ i 1) (cons (gen-elem) acc)))))))

(define (gen-pair gen-a gen-b)
  (lambda ()
    (cons (gen-a) (gen-b))))

;; ============================================================================
;; Arithmetic Properties
;; ============================================================================

(display "=== Arithmetic Properties ===\n")

(check "addition is commutative"
  (lambda (p) (= (+ (car p) (cdr p)) (+ (cdr p) (car p))))
  (gen-pair gen-int gen-int))

(check "addition is associative"
  (lambda (t) 
    (let ((a (car t)) (b (cadr t)) (c (caddr t)))
      (= (+ (+ a b) c) (+ a (+ b c)))))
  (lambda () (list (gen-int) (gen-int) (gen-int))))

(check "multiplication is commutative"
  (lambda (p) (= (* (car p) (cdr p)) (* (cdr p) (car p))))
  (gen-pair gen-int gen-int))

(check "multiplication distributes over addition"
  (lambda (t)
    (let ((a (car t)) (b (cadr t)) (c (caddr t)))
      (= (* a (+ b c)) (+ (* a b) (* a c)))))
  (lambda () (list (gen-int) (gen-int) (gen-int))))

(check "x + 0 = x"
  (lambda (x) (= (+ x 0) x))
  gen-int)

(check "x * 1 = x"
  (lambda (x) (= (* x 1) x))
  gen-int)

(check "x * 0 = 0"
  (lambda (x) (= (* x 0) 0))
  gen-int)

(check "x - x = 0"
  (lambda (x) (= (- x x) 0))
  gen-int)

(check "negation: (- (- x)) = x"
  (lambda (x) (= (- (- x)) x))
  gen-int)

(check "abs is non-negative"
  (lambda (x) (>= (abs x) 0))
  gen-int)

(check "abs(x) = abs(-x)"
  (lambda (x) (= (abs x) (abs (- x))))
  gen-int)

;; ============================================================================
;; List Properties
;; ============================================================================

(display "\n=== List Properties ===\n")

(check "length is non-negative"
  (lambda (lst) (>= (length lst) 0))
  (gen-list gen-int))

(check "reverse preserves length"
  (lambda (lst) (= (length (reverse lst)) (length lst)))
  (gen-list gen-int))

(check "reverse(reverse(x)) = x"
  (lambda (lst) (equal? (reverse (reverse lst)) lst))
  (gen-list gen-int))

(check "append length is sum of lengths"
  (lambda (p)
    (let ((a (car p)) (b (cdr p)))
      (= (length (append a b)) (+ (length a) (length b)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "append '() x = x"
  (lambda (lst) (equal? (append '() lst) lst))
  (gen-list gen-int))

(check "append x '() = x"
  (lambda (lst) (equal? (append lst '()) lst))
  (gen-list gen-int))

(check "map preserves length"
  (lambda (lst) (= (length (map (lambda (x) (* x 2)) lst)) (length lst)))
  (gen-list gen-int))

(check "filter result <= original length"
  (lambda (lst) (<= (length (filter even? lst)) (length lst)))
  (gen-list gen-int))

(check "car of cons is first element"
  (lambda (p) (equal? (car (cons (car p) (cdr p))) (car p)))
  (gen-pair gen-int (gen-list gen-int)))

(check "cdr of cons is second element"
  (lambda (p) (equal? (cdr (cons (car p) (cdr p))) (cdr p)))
  (gen-pair gen-int (gen-list gen-int)))

;; ============================================================================
;; Comparison Properties
;; ============================================================================

(display "\n=== Comparison Properties ===\n")

(check "x = x (reflexive)"
  (lambda (x) (= x x))
  gen-int)

(check "if x < y then not y < x"
  (lambda (p)
    (let ((x (car p)) (y (cdr p)))
      (if (< x y) (not (< y x)) #t)))
  (gen-pair gen-int gen-int))

(check "exactly one of <, =, > holds"
  (lambda (p)
    (let ((x (car p)) (y (cdr p)))
      (= 1 (+ (if (< x y) 1 0)
              (if (= x y) 1 0)
              (if (> x y) 1 0)))))
  (gen-pair gen-int gen-int))

(check "min(x,y) <= x and min(x,y) <= y"
  (lambda (p)
    (let ((x (car p)) (y (cdr p)) (m (min (car p) (cdr p))))
      (and (<= m x) (<= m y))))
  (gen-pair gen-int gen-int))

(check "max(x,y) >= x and max(x,y) >= y"
  (lambda (p)
    (let ((x (car p)) (y (cdr p)) (m (max (car p) (cdr p))))
      (and (>= m x) (>= m y))))
  (gen-pair gen-int gen-int))

;; ============================================================================
;; Division Properties
;; ============================================================================

(display "\n=== Division Properties ===\n")

(check "quotient * divisor + remainder = dividend"
  (lambda (p)
    (let ((a (car p)) (b (cdr p)))
      (if (= b 0)
          #t  ; skip division by zero
          (= a (+ (* (quotient a b) b) (remainder a b))))))
  (gen-pair gen-int gen-int))

(check "modulo result has same sign as divisor"
  (lambda (p)
    (let ((a (car p)) (b (cdr p)))
      (if (= b 0)
          #t
          (let ((m (modulo a b)))
            (or (= m 0)
                (if (> b 0) (> m 0) (< m 0)))))))
  (gen-pair gen-int gen-int))

(check "gcd divides both arguments"
  (lambda (p)
    (let ((a (car p)) (b (cdr p)))
      (if (and (= a 0) (= b 0))
          #t
          (let ((g (gcd a b)))
            (and (= (modulo a g) 0)
                 (= (modulo b g) 0))))))
  (gen-pair gen-int gen-int))

;; ============================================================================
;; String Properties
;; ============================================================================

(display "\n=== String Properties ===\n")

(define (gen-string)
  (list->string 
    (let ((len (random-integer 20)))
      (let loop ((i 0) (acc '()))
        (if (>= i len)
            acc
            (loop (+ i 1) 
                  (cons (integer->char (+ 97 (random-integer 26))) acc)))))))

(check "string-length of string->list equals list length"
  (lambda (s) (= (string-length s) (length (string->list s))))
  gen-string)

(check "list->string(string->list(s)) = s"
  (lambda (s) (string=? (list->string (string->list s)) s))
  gen-string)

(check "string-append length is sum"
  (lambda (p)
    (let ((a (car p)) (b (cdr p)))
      (= (string-length (string-append a b))
         (+ (string-length a) (string-length b)))))
  (gen-pair gen-string gen-string))

;; ============================================================================
;; Vector Properties  
;; ============================================================================

(display "\n=== Vector Properties ===\n")

(define (gen-vector)
  (let ((len (random-integer 30)))
    (let ((v (make-vector len 0)))
      (let loop ((i 0))
        (if (>= i len)
            v
            (begin
              (vector-set! v i (gen-int))
              (loop (+ i 1))))))))

(check "vector-length of list->vector equals list length"
  (lambda (lst) (= (vector-length (list->vector lst)) (length lst)))
  (gen-list gen-int))

(check "vector->list(list->vector(x)) = x"
  (lambda (lst) (equal? (vector->list (list->vector lst)) lst))
  (gen-list gen-int))

(check "list->vector(vector->list(v)) equals v element-wise"
  (lambda (v)
    (let ((v2 (list->vector (vector->list v))))
      (let loop ((i 0))
        (if (>= i (vector-length v))
            #t
            (if (= (vector-ref v i) (vector-ref v2 i))
                (loop (+ i 1))
                #f)))))
  gen-vector)

;; ============================================================================
;; Higher-Order Function Properties
;; ============================================================================

(display "\n=== Higher-Order Function Properties ===\n")

(check "map id = id"
  (lambda (lst) (equal? (map (lambda (x) x) lst) lst))
  (gen-list gen-int))

(check "map(f, map(g, x)) = map(compose f g, x)"
  (lambda (lst)
    (let ((f (lambda (x) (* x 2)))
          (g (lambda (x) (+ x 1))))
      (equal? (map f (map g lst))
              (map (lambda (x) (f (g x))) lst))))
  (gen-list gen-int))

(check "filter true keeps all"
  (lambda (lst) (equal? (filter (lambda (x) #t) lst) lst))
  (gen-list gen-int))

(check "filter false keeps none"
  (lambda (lst) (null? (filter (lambda (x) #f) lst)))
  (gen-list gen-int))

(check "fold-right cons '() = identity"
  (lambda (lst) (equal? (fold-right cons '() lst) lst))
  (gen-list gen-int))

(check "fold + 0 = sum"
  (lambda (lst)
    (let ((sum (fold + 0 lst)))
      (= sum (apply + (cons 0 lst)))))
  (gen-list gen-int))

;; ============================================================================
;; GC Stress Properties (tests that GC preserves values)
;; ============================================================================

(display "\n=== GC Stress Properties ===\n")

(check "deeply nested list survives operations"
  (lambda (lst)
    (let* ((nested (map list (map list lst)))
           (len1 (length nested))
           (flattened (apply append (apply append nested)))
           (len2 (length flattened)))
      (= len2 (length lst))))
  (gen-list gen-int))

(check "closure captures survive allocation"
  (lambda (vals)
    (let ((closures (map (lambda (x) (lambda () x)) vals)))
      ;; Force allocation between closure creation and use
      (let ((filler (make-list 1000)))
        (equal? vals (map (lambda (f) (f)) closures)))))
  (gen-list gen-int))

(check "continuation values preserved across GC"
  (lambda (n)
    (let ((result 0))
      (call/cc
        (lambda (k)
          ;; Force allocation
          (let ((filler (make-list 500)))
            (set! result n)
            (k n))))
      (= result n)))
  gen-int)

;; ============================================================================
;; Summary
;; ============================================================================

(newline)
(display "========================================\n")
(display "Property Tests: ")
(display (+ *passed* *failed*))
(display ", Passed: ")
(display *passed*)
(display ", Failed: ")
(display *failed*)
(newline)
(display "Total individual checks: ")
(display *total-checks*)
(newline)

(if (= *failed* 0)
    (display "All property tests passed!\n")
    (begin
      (display "SOME TESTS FAILED!\n")
      (exit 1)))
