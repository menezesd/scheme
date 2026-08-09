;;; Property-Based Tests for Scheme Interpreter
;;; Uses random generation to test invariants

;; Set random seed for reproducibility
(random-seed! 12345)

;; Configuration
(define *num-tests* 200)
(define *max-list-size* 30)
(define *max-int* 5000)

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
;; Map Properties (comprehensive functor laws)
;; ============================================================================

(display "\n=== Map Properties ===\n")

;; Already tested above: map id = id, map composition

(check "map length preservation"
  (lambda (lst)
    (= (length (map (lambda (x) (* x 2)) lst)) (length lst)))
  (gen-list gen-int))

(check "map pointwise correctness"
  (lambda (lst)
    (let ((f (lambda (x) (+ x 10)))
          (result (map (lambda (x) (+ x 10)) lst)))
      (let loop ((i 0) (xs lst) (ys result))
        (cond ((null? xs) (null? ys))
              ((null? ys) #f)
              ((not (= (f (car xs)) (car ys))) #f)
              (else (loop (+ i 1) (cdr xs) (cdr ys)))))))
  (gen-list gen-int))

(check "map distributes over append"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p))
          (f (lambda (x) (* x 2))))
      (equal? (map f (append xs ys))
              (append (map f xs) (map f ys)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "map interaction with reverse"
  (lambda (lst)
    (let ((f (lambda (x) (* x 2))))
      (equal? (map f (reverse lst))
              (reverse (map f lst)))))
  (gen-list gen-int))

(check "map interaction with filter"
  (lambda (lst)
    (let ((f (lambda (x) (* x 2)))
          (p (lambda (x) (even? x))))
      (equal? (filter p (map f lst))
              (map f (filter (lambda (x) (p (f x))) lst)))))
  (gen-list gen-int))

;; Multi-list map
(check "map with two lists - length is minimum"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)))
      (= (length (map + xs ys))
         (min (length xs) (length ys)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "map with two lists - pointwise correctness"
  (lambda (p)
    (let* ((xs (car p)) (ys (cdr p))
           (result (map + xs ys))
           (minlen (min (length xs) (length ys))))
      (let loop ((i 0) (xs xs) (ys ys) (rs result))
        (if (>= i minlen)
            (null? rs)
            (and (not (null? rs))
                 (= (car rs) (+ (car xs) (car ys)))
                 (loop (+ i 1) (cdr xs) (cdr ys) (cdr rs)))))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

;; ============================================================================
;; Filter Properties
;; ============================================================================

(display "\n=== Filter Properties ===\n")

;; Already tested: filter true, filter false

(check "filter keeps only matching elements"
  (lambda (lst)
    (let ((result (filter even? lst)))
      (let loop ((xs result))
        (if (null? xs) #t
            (and (even? (car xs)) (loop (cdr xs)))))))
  (gen-list gen-int))

(check "filter preserves order (subsequence)"
  (lambda (lst)
    (let ((result (filter even? lst)))
      (let loop ((orig lst) (filt result))
        (cond ((null? filt) #t)
              ((null? orig) #f)
              ((= (car orig) (car filt)) (loop (cdr orig) (cdr filt)))
              (else (loop (cdr orig) filt))))))
  (gen-list gen-int))

(check "filter is idempotent"
  (lambda (lst)
    (equal? (filter even? (filter even? lst))
            (filter even? lst)))
  (gen-list gen-int))

(check "filter distributes over append"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)))
      (equal? (filter even? (append xs ys))
              (append (filter even? xs) (filter even? ys)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "filter + negated filter = full length"
  (lambda (lst)
    (= (+ (length (filter even? lst))
          (length (filter odd? lst)))
       (length lst)))
  (gen-list gen-int))

(check "filter matches spec"
  (lambda (lst)
    (letrec ((filter-spec (lambda (p xs)
               (if (null? xs) '()
                   (if (p (car xs))
                       (cons (car xs) (filter-spec p (cdr xs)))
                       (filter-spec p (cdr xs)))))))
      (equal? (filter even? lst)
              (filter-spec even? lst))))
  (gen-list gen-int))

;; ============================================================================
;; Append Properties
;; ============================================================================

(display "\n=== Append Properties ===\n")

;; Already tested above: append length is sum

(check "append '() x = x"
  (lambda (lst) (equal? (append '() lst) lst))
  (gen-list gen-int))

(check "append x '() = x"
  (lambda (lst) (equal? (append lst '()) lst))
  (gen-list gen-int))

(check "append is associative"
  (lambda (t)
    (let ((xs (car t)) (ys (cadr t)) (zs (caddr t)))
      (equal? (append (append xs ys) zs)
              (append xs (append ys zs)))))
  (lambda () (list ((gen-list gen-int)) ((gen-list gen-int)) ((gen-list gen-int)))))

(check "append head is first list's head"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)))
      (if (null? xs)
          #t
          (= (car (append xs ys)) (car xs)))))
  (gen-pair (gen-nonempty-list gen-int) (gen-list gen-int)))

(check "append tail recursion"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)))
      (if (null? xs)
          (equal? (append xs ys) ys)
          (equal? (cdr (append xs ys))
                  (append (cdr xs) ys)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "reverse(append xs ys) = append(reverse ys, reverse xs)"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)))
      (equal? (reverse (append xs ys))
              (append (reverse ys) (reverse xs)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

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
;; Append Spine/Sharing Properties
;; ============================================================================

(display "\n=== Append Spine/Sharing Properties ===\n")

(check "append preserves prefix"
  (lambda (p)
    (let ((xs (car p)) (ys (cdr p)) (zs (append (car p) (cdr p))))
      (let loop ((a xs) (b zs))
        (cond ((null? a) #t)
              ((null? b) #f)
              ((equal? (car a) (car b)) (loop (cdr a) (cdr b)))
              (else #f)))))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "append preserves suffix"
  (lambda (p)
    (let* ((xs (car p)) (ys (cdr p))
           (zs (append xs ys)))
      (equal? (list-tail zs (length xs)) ys)))
  (gen-pair (gen-list gen-int) (gen-list gen-int)))

(check "append copies first list spine (mutating xs doesn't affect result)"
  (lambda (p)
    (let* ((xs (car p)) (ys (cdr p)))
      (if (null? xs)
          #t
          (let* ((zs (append xs ys))
                 (old (car zs)))
            (set-car! xs (+ (car xs) 1))
            (= (car zs) old)))))
  (gen-pair (gen-nonempty-list gen-int) (gen-list gen-int)))

(check "append shares tail (mutating ys affects result tail)"
  (lambda (p)
    (let* ((xs (car p)) (ys (cdr p)))
      (if (null? ys)
          #t
          (let* ((zs (append xs ys))
                 (tail (list-tail zs (length xs)))
                 (old (car tail)))
            (set-car! ys (+ (car ys) 1))
            (not (= (car tail) old))))))
  (gen-pair (gen-list gen-int) (gen-nonempty-list gen-int)))

;; ============================================================================
;; Map/Filter Spine and Call-Count Properties
;; ============================================================================

(display "\n=== Map/Filter Spine Properties ===\n")

(check "map produces fresh spine (no sharing with input cells)"
  (lambda (lst)
    (if (null? lst) #t
        (let* ((ys (map (lambda (x) x) lst))
               (old (car ys)))
          (set-car! lst (+ (car lst) 1))
          (= (car ys) old))))
  (gen-nonempty-list gen-int))

(check "filter produces fresh spine (no sharing with input cells)"
  (lambda (lst)
    (let ((ys (filter even? lst)))
      (if (null? ys) #t
          (let ((old (car ys)))
            ;; find & mutate first even cell in lst (if any)
            (let loop ((xs lst))
              (cond ((null? xs) #t)
                    ((even? (car xs))
                     (set-car! xs (+ (car xs) 1))
                     (= (car ys) old))
                    (else (loop (cdr xs)))))))))
  (gen-list gen-int))

(check "map calls f exactly length(xs) times"
  (lambda (lst)
    (let ((count 0))
      (map (lambda (x) (set! count (+ count 1)) x) lst)
      (= count (length lst))))
  (gen-list gen-int))

(check "filter calls predicate exactly length(xs) times"
  (lambda (lst)
    (let ((count 0))
      (filter (lambda (x) (set! count (+ count 1)) (even? x)) lst)
      (= count (length lst))))
  (gen-list gen-int))

(check "filter composition"
  (lambda (lst)
    (let ((p even?) (q (lambda (x) (> x 0))))
      (equal? (filter p (filter q lst))
              (filter (lambda (x) (and (q x) (p x))) lst))))
  (gen-list gen-int))

(check "map exactly-once under allocation pressure"
  (lambda (lst)
    (let ((count 0))
      (map (lambda (x)
             (let ((junk (make-list 200))) ; allocate
               (set! count (+ count 1))
               (+ x 1)))
           lst)
      (= count (length lst))))
  (gen-list gen-int))

;; ============================================================================
;; Shared Structure and Multi-Shot Continuation Properties
;; ============================================================================

(display "\n=== Shared Structure Properties ===\n")

(check "shared substructure remains shared after allocation"
  (lambda (lst)
    (let* ((shared (cons lst lst))
           (a (car shared))
           (b (cdr shared))
           (junk (make-list 10000)))
      (eq? a b)))
  (gen-list gen-int))

(check "call/cc can be invoked multiple times"
  (lambda (n)
    (let ((k #f) (x 0))
      (let ((r (call/cc (lambda (kk) (set! k kk) 0))))
        (set! x (+ x 1))
        (if (< x 3) (k r) (= x 3)))))
  gen-int)

;; ============================================================================
;; Numeric and Type Properties
;; ============================================================================

(display "\n=== Numeric Properties ===\n")

(check "= implies not < and not >"
  (lambda (p)
    (let ((x (car p)) (y (cdr p)))
      (if (= x y) (and (not (< x y)) (not (> x y))) #t)))
  (gen-pair gen-int gen-int))

(check "apply + equals fold +"
  (lambda (lst)
    (= (apply + (cons 0 lst))
       (fold + 0 lst)))
  (gen-list gen-int))

(check "apply list equals identity"
  (lambda (lst) (equal? (apply list lst) lst))
  (gen-list gen-int))

;; ============================================================================
;; Vector Mutation Properties
;; ============================================================================

(display "\n=== Vector Mutation Properties ===\n")

(check "vector-set! then vector-ref gives same"
  (lambda (v)
    (if (= (vector-length v) 0) #t
        (let* ((i (random-integer (vector-length v)))
               (x (gen-int)))
          (vector-set! v i x)
          (= (vector-ref v i) x))))
  gen-vector)

(check "make-vector shares fill object (pairs)"
  (lambda (n)
    (let* ((p (cons 1 2))
           (v (make-vector 5 p)))
      (set-car! (vector-ref v 0) 99)
      (= (car (vector-ref v 4)) 99)))
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
