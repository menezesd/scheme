;;; Workload for `make test-gcstress`.
;;;
;;; Run with VESPER_GC_STRESS=N, which forces a minor collection every N
;;; allocations instead of waiting for the 256K-cell nursery to fill. The
;;; point is not the assertions below - it is that loading the stdlib and
;;; exercising the compiler at all is enough to catch a cell reference that
;;; was held across a collection without being rooted.
;;;
;;; Keep this file small. Every collection copies the live set, so a stress
;;; run is orders of magnitude slower than a normal one.

(define failures 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (begin (display "  PASS: ") (display name) (newline))
      (begin
        (set! failures (+ failures 1))
        (display "  FAIL: ") (display name)
        (display " - expected ") (write expected)
        (display " got ") (write actual) (newline))))

;; Named-let loops go through the compiler's loop optimization, which reads
;; the loop parameter list out of the compile context. A stale copy there
;; emits SETs against the wrong symbols and the loop never terminates.
(check "named let accumulates" 15
       (let loop ((i 1) (acc 0)) (if (> i 5) acc (loop (+ i 1) (+ acc i)))))
(check "named let over a list" '(3 2 1)
       (let loop ((l '(1 2 3)) (acc '()))
         (if (null? l) acc (loop (cdr l) (cons (car l) acc)))))
(check "nested named lets" 30
       (let outer ((i 0) (total 0))
         (if (= i 3)
             total
             (outer (+ i 1)
                    (+ total (let inner ((j 0) (s 0))
                               (if (= j 5) s (inner (+ j 1) (+ s j)))))))))

;; for-each and map are the stdlib's own loop-optimized traversals.
(check "for-each visits every element" '(3 2 1)
       (let ((seen '()))
         (for-each (lambda (x) (set! seen (cons x seen))) '(1 2 3))
         seen))
(check "map over a list" '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))
(check "map over two lists" '(5 7 9) (map + '(1 2 3) '(4 5 6)))
(check "assoc finds a pair" '(2 . b) (assoc 2 '((1 . a) (2 . b))))

;; Macro expansion mints gensyms and defines them into the compile-time
;; environment, which is where two of the rooting bugs lived.
(define-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(check "hygienic swap" '(2 1)
       (let ((x 1) (y 2)) (swap! x y) (list x y)))
(check "macro in a loop" 10
       (let loop ((i 0) (acc 0))
         (if (= i 5) acc (loop (+ i 1) (let ((t i)) (+ acc t))))))

;; Allocation-heavy work, so collections land mid-construction.
(check "string round trip" "abcabc" (string-append "abc" (string-copy "abc")))
(check "vector build" '#(0 1 4 9)
       (let ((v (make-vector 4 0)))
         (let loop ((i 0)) (if (< i 4) (begin (vector-set! v i (* i i)) (loop (+ i 1)))))
         v))
(check "bignum arithmetic" 1267650600228229401496703205376 (expt 2 100))
(check "rational arithmetic" 5/6 (+ 1/2 1/3))
(check "sort a list" '(1 2 3 4 5) (sort '(3 1 4 5 2) <))
(check "deep-ish recursion" 5050
       (let ()
         (define (sum n) (if (= n 0) 0 (+ n (sum (- n 1)))))
         (sum 100)))

;; Continuations and dynamic-wind copy the operand stack, another place a
;; cell reference can go stale.
(check "call/cc escape" 42 (call-with-current-continuation (lambda (k) (+ 1 (k 42)))))
(check "dynamic-wind order" '(in body out)
       (let ((r '()))
         (dynamic-wind (lambda () (set! r (cons 'in r)))
                       (lambda () (set! r (cons 'body r)))
                       (lambda () (set! r (cons 'out r))))
         (reverse r)))
(check "guard catches" 'caught (guard (e (#t 'caught)) (raise 'boom)))

;; The CPS interpreter's quasiquote expander builds its result by walking a
;; reversed copy of the template and consing/appending each transformed
;; element. Every step allocates, so an unrooted cursor into that copy reads
;; reclaimed cells after a collection - the output once contained [builtin]
;; objects spliced in as elements. Nested quasiquote makes the walk deep
;; enough for a stress collection to land inside it.
(check "nested quasiquote with unquote" '(a (quasiquote (b (unquote x) (unquote (quote y)) d)) e)
       (let ((name1 'x) (name2 'y))
         `(a `(b ,,name1 ,',name2 d) e)))
(check "quasiquote splicing" '(1 2 3 4 5)
       (let ((mid '(2 3 4))) `(1 ,@mid 5)))
(check "quasiquote vector" '#(1 2 3)
       (let ((two 2)) `#(1 ,two 3)))

;; R7RS import environments are built by walking the pristine stdlib
;; environment and defvar-ing selected bindings into a fresh one. Those
;; cursors point into old-generation cells, so only a *major* collection
;; landing mid-walk exposes an unrooted one - a minor collection never moves
;; them. This is why test-gcstress runs VESPER_GC_STRESS_MAJOR intervals too.
(check "environment import walks the stdlib env" 5
       (eval '(+ 2 3) (environment '(scheme base))))
(check "environment import isolates other bindings" #t
       (guard (e (#t #t))
         (eval 'no-such-binding-here (environment '(scheme base)))
         #f))
(check "scheme-report-environment evaluates" '(2 3)
       (let ((r (scheme-report-environment 5)))
         (list (eval '(length (map (lambda (x) (+ x 1)) '(1 2))) r)
               (eval '(+ 1 2) r))))

;; TRMC keeps its accumulator in two operand-stack slots rather than in
;; vm_state, on the theory that the collectors already trace the whole stack.
;; TRMC_APPEND allocates a cell per element, so at a forced collection per
;; allocation every single iteration collects between reading the head/tail
;; slots and writing them back. If that reasoning is wrong this is where it
;; shows: a stale head returns a truncated or corrupted list.
(check "trmc list length under forced collections" 200
       (length (let loop ((k 200)) (if (= k 0) '() (cons k (loop (- k 1)))))))
(check "trmc list contents under forced collections" '(4 3 2 1)
       (let loop ((k 4)) (if (= k 0) '() (cons k (loop (- k 1))))))
(check "trmc dotted tail survives collection" '(2 1 . end)
       (let loop ((k 2)) (if (= k 0) 'end (cons k (loop (- k 1))))))
(check "trmc element expression allocates too" '((3) (2) (1))
       (let loop ((k 3)) (if (= k 0) '() (cons (list k) (loop (- k 1))))))

;; The append shape copies a spine per iteration, so TRMC_SPLICE allocates
;; several cells per element rather than one - more chances for a collection
;; to land mid-copy, with the accumulator half-linked.
(check "trmc append splices under forced collections" '(3 3 2 2 1 1)
       (let loop ((k 3)) (if (= k 0) '() (append (list k k) (loop (- k 1))))))
(check "trmc append length under forced collections" 40
       (length (let loop ((k 20))
                 (if (= k 0) '() (append (list k k) (loop (- k 1)))))))

;; The functional accumulator allocates a cell per level while building and
;; then applies the operator once per level at the return, so a forced
;; collection lands on both halves. The cursor walking it during the replay is
;; the accumulator's own stack slot, which is what makes it survive.
(check "trmc fold rebuilds a list under forced collections" '(30 20 10)
       (let ((scale (lambda (x) (* x 10))))
         (let loop ((k 3))
           (if (= k 0) '() (cons (scale k) (loop (- k 1)))))))
(check "trmc fold string-append under forced collections" "3,2,1,"
       (let loop ((k 3))
         (if (= k 0) "" (string-append (number->string k) "," (loop (- k 1))))))
(check "trmc fold arithmetic under forced collections" 210
       (let loop ((k 20)) (if (= k 0) 0 (+ k (loop (- k 1))))))

;; The same accumulator reached through cond and through a let frame - the
;; let case also unwinds an environment frame on every iteration, so a
;; collection can land between the unwind and the jump.
(check "trmc under cond with forced collections" '(4 3 2 1)
       (let loop ((k 4))
         (cond ((= k 0) '()) (else (cons k (loop (- k 1)))))))
(check "trmc under a let frame with forced collections" '(8 6 4 2)
       (let loop ((k 4))
         (if (= k 0) '() (let ((d (* k 2))) (cons d (loop (- k 1)))))))

(newline)
(display "GC stress tests: ")
(display (if (= failures 0) "all passed" "FAILURES"))
(newline)
(if (> failures 0) (exit 1))
