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
(check "map stops before an unused dotted tail under collection" '(5 7)
       (map (lambda (x y) (gc-flip) (+ x y)) '(1 2) '(4 5 . tail)))
(check "map continuations retain immutable accumulated prefixes" '((1 2 3) (1 20 3))
       (let ((saved #f) (first #f) (again? #f))
         (let ((result (map (lambda (x)
                              (if (= x 2)
                                  (call/cc (lambda (k) (set! saved k) x))
                                  x))
                            '(1 2 3))))
           (if again?
               (list first result)
               (begin (set! first result) (set! again? #t)
                      (gc-flip) (saved 20))))))
(check "assoc finds a pair" '(2 . b) (assoc 2 '((1 . a) (2 . b))))
(check "destructive partition survives allocating predicates" '((2 4) (1 3))
       (call-with-values
         (lambda ()
           (partition! (lambda (x) (gc-flip) (even? x)) (list 1 2 3 4)))
         list))
(check "circular searches survive allocating predicates" '(2 #f)
       (let ((cycle (circular-list 1 2 3)))
         (list (any (lambda (x) (gc-flip) (and (even? x) x)) cycle)
               (every (lambda (x) (gc-flip) (odd? x)) cycle))))
(check "generated symbols retain identity across collection" '(#f #t)
       (let* ((fresh (gensym))
              (ordinary (string->symbol (symbol->string fresh))))
         (gc-flip)
         (list (eq? fresh ordinary)
               (eq? ordinary (string->symbol (symbol->string fresh))))))
(check "equivalent cyclic hash keys survive collection" '(#t 42 42)
       (let ((table (make-hash-table))
             (one (circular-list 'x))
             (two (circular-list 'x 'x)))
         (hash-table-set! table one 42)
         (gc-flip)
         (list (= (hash one) (hash two))
               (hash-table-ref table two)
               (hash-table-ref (hash-table-copy table) two))))

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
(check "nested string-slices retain their original through collections" "Xλ"
       (let ((slice (string-slice (string-slice (string-copy "aé𝄞def") 1 5) 1 3)))
         (gc-flip)
         (string-set! slice 0 #\X)
         (string-set! slice 1 #\λ)
         slice))
(check "vector build" '#(0 1 4 9)
       (let ((v (make-vector 4 0)))
         (let loop ((i 0)) (if (< i 4) (begin (vector-set! v i (* i i)) (loop (+ i 1)))))
         v))
(check "bignum arithmetic" 1267650600228229401496703205376 (expt 2 100))
(check "rational arithmetic" 5/6 (+ 1/2 1/3))
(check "unary complex arithmetic keeps rational signs" '(-1/2-3/4i 8/13-12/13i)
       (list (- 1/2+3/4i) (/ 1/2+3/4i)))
(check "complex conversion preserves exact components" 1/3+1/2i
       (inexact->exact (make-rectangular 1/3 0.5)))
(check "complex conversion roots both components" #t
       (= (inexact->exact (make-rectangular 0.1 0.2))
          (make-rectangular (inexact->exact 0.1) (inexact->exact 0.2))))
(check "complex math results survive collections" '(#t -2.0 0.0 1.0)
       (list (finite? (atan 1+1i))
             (imag-part (sqrt (make-rectangular -4.0 -0.0)))
             (expt 0.0+0.0i 2)
             (expt 0.0+0.0i 0)))
(check "nonfinite complex text survives collections" '(#t #t)
       (list (infinite? (real-part (string->number "+inf.0+1/3i")))
             (nan? (imag-part (string->number "1/3+nan.0i")))))
(check "numeric prefixes preserve complex components across collections"
       '(1234567890123456789/1000000000000000000+1i 30.0+10.0i 0.0+1.0i)
       (list (string->number "#e1.234567890123456789+1.0i")
             (read (open-input-string "#x#i1e+ai"))
             (read (open-input-string "#i+i"))))
(check "complex radix formatting roots both components" #t
       (let ((z (make-rectangular 12345678901234567890/7 -9/11)))
         (eqv? z (string->number (number->string z 16) 16))))
(check "rationalize exact bounds survive collections" '(1/1000 -1/1000 355/113)
       (list (rationalize 1/1000 1/1000000)
             (rationalize -1/1000 -1/1000000)
             (rationalize 314159265/100000000 1/1000000)))
(check "promise aliases share memoized state across collections" '(42 42 1)
       (let ((count 0))
         (let* ((p (delay (begin (set! count (+ count 1)) 42)))
                (q (delay-force p)))
           (let ((value (force q)))
             (gc-flip)
             (list value (force p) count)))))
(check "parameterize evaluates values before rebinding" '(10 1)
       (let ((p (make-parameter 1)) (q (make-parameter 2)))
         (parameterize ((p 10) (q (p))) (list (p) (q)))))
(check "nested definitions shadow outer syntax before use" 42
       (let-syntax ((h (syntax-rules () ((_) 99))))
         (letrec ((f (lambda (x)
                       (define (g) (h))
                       (begin (begin (define (h) x)))
                       (g))))
           (f 42))))
(check "record procedures retain their type across rebinding and GC" 2
       (let ()
         (define-record-type thing (make-thing x) thing? (x thing-x set-thing-x!))
         (let ((constructor make-thing))
           (set! make-thing #f)
           (set! thing? #f)
           (gc-flip)
           (let ((obj (constructor 1)))
             (set-thing-x! obj 2)
             (thing-x obj)))))
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
(check "current port parameters survive collection and restore identity" '("42" #t)
       (let ((old (current-output-port)))
         (let ((result
                 (parameterize ((current-output-port (open-output-string)))
                   (gc-flip)
                   (display 42)
                   (get-output-string (current-output-port)))))
           (list result (eq? old (current-output-port))))))
(check "closed output string port retains its buffer through collection" "λ𝄞"
       (let ((p (open-output-string)))
         (display "λ𝄞" p)
         (close-output-port p)
         (gc-flip)
         (get-output-string p)))
(check "polar reader retains allocating components under collection" #t
       (= (string->number "12345678901234567890/7@1/2")
          (make-polar 12345678901234567890/7 1/2)))
(check "macro substitution preserves cyclic data under collection" #t
       (let ((x '#1=#2=#(#1#)))
         (gc-flip)
         (eq? (vector-ref x 0) x)))

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
(check "imported macro retains private bindings under collection" 6
       (eval '(let ((p (make-parameter 5))) (parameterize ((p 6)) (p)))
             (environment '(scheme base))))
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

(check "rebound recursive call preserves captured prefixes" '((2 a) (2 b))
       (let ((saved #f) (first #f) (phase 0))
         (letrec ((f (lambda (n)
                       (if (= n 0) '() (cons n (f (- n 1)))))))
           (let ((old f))
             (set! f (lambda (n)
                       (call/cc (lambda (k) (set! saved k) '(a)))))
             (let ((answer (old 2)))
               (if (= phase 0)
                   (begin (set! first answer) (set! phase 1) (saved '(b)))
                   (list first answer)))))))

(check "rebound primitive captures deferred operations" '((3 2 1) (3 x))
       (let ((original-cons cons) (saved #f) (first #f) (phase 0))
         (letrec ((f (lambda (n)
                       (if (= n 0) '() (cons n (f (- n 1)))))))
           (set! cons
                 (lambda (a b)
                   (call/cc (lambda (k)
                              (if (= a 2) (set! saved k))
                              (original-cons a b)))))
           (let ((answer (f 3)))
             (if (= phase 0)
                 (begin (set! first answer) (set! phase 1) (saved '(x)))
                 (begin (set! cons original-cons) (list first answer)))))))

;; Both collectors copy a cell first and trace its references later, from
;; their scan loops, so a structure's nesting depth is bounded by the heap
;; rather than the C stack. The old recursive walk overflowed on a chain about
;; 300K deep (a linked list of records - records are vectors); building one is
;; far too slow under forced collections, so these only check that records,
;; vectors and hash tables reached through one another still come through both
;; collectors whole.
(define-record-type gc-node (make-gc-node val next) gc-node?
  (val gc-node-val) (next gc-node-next))
(check "record chain survives collection" 4950
       (let ((chain (let loop ((i 0) (acc '()))
                      (if (= i 100) acc (loop (+ i 1) (make-gc-node (list i) acc))))))
         (gc-flip)
         (let loop ((l chain) (sum 0))
           (if (gc-node? l)
               (loop (gc-node-next l) (+ sum (car (gc-node-val l))))
               sum))))
(check "nested vectors survive collection" 50
       (let ((v (let loop ((i 0) (v '()))
                  (if (= i 50) v (loop (+ i 1) (vector v (list i)))))))
         (gc-flip)
         (let loop ((v v) (d 0))
           (if (vector? v) (loop (vector-ref v 0) (+ d 1)) d))))
(check "nested hash tables survive collection" '(49 . 0)
       (let ((t (let loop ((i 0) (t #f))
                  (if (= i 50)
                      t
                      (let ((h (make-strong-eqv-hash-table)))
                        (hash-table-set! h 'next t)
                        (hash-table-set! h 'val (list i))
                        (loop (+ i 1) h))))))
         (gc-flip)
         (cons (car (hash-table-ref/default t 'val #f))
               (let loop ((t t))
                 (let ((next (hash-table-ref/default t 'next #f)))
                   (if next (loop next) (car (hash-table-ref/default t 'val #f))))))))
;; eqv tables hash a vector key by its cell index, so every table whose keys
;; moved must be rehashed after the collection; the rehash is now registered
;; when the scan loop reaches the table rather than when it is copied.
(check "eqv table keyed by moved vectors still finds them" #t
       (let* ((keys (let loop ((i 0) (acc '()))
                      (if (= i 20) acc (loop (+ i 1) (cons (vector i) acc)))))
              (t (make-strong-eqv-hash-table))
              (holder (vector t)))
         (for-each (lambda (k) (hash-table-set! t k (vector-ref k 0))) keys)
         (gc-flip)
         (let loop ((ks keys))
           (or (null? ks)
               (and (eqv? (hash-table-ref/default (vector-ref holder 0) (car ks) #f)
                          (vector-ref (car ks) 0))
                    (loop (cdr ks)))))))

;; Records are vector payloads under their own cell type. An old record
;; mutated to point at a fresh object is only found by the minor collection's
;; dirty-card scan; error objects, promises, bytevector ports and char-sets
;; are records too, and the C runtime builds error objects itself.
(check "an old record's field set to fresh data survives collections"
       '(0 1 2 3 4 5 6 7 8 9)
       (let ()
         (define-record-type box (make-box value) box?
           (value box-value set-box-value!))
         (let ((b (make-box #f)))
           (gc-flip)
           (set-box-value! b (iota 10))
           (let churn ((i 0))
             (if (< i 2000) (begin (cons i i) (churn (+ i 1)))))
           (box-value b))))
(check "the procedural record API survives collections" '(#f 9 (1 #f 3))
       (let* ((q (make-record-type 'q '(a b c)))
              (make-q2 (record-constructor q '(b)))
              (make-q3 (record-constructor q '(c a))))
         (gc-flip)
         (let ((r (make-q2 9)) (s (make-q3 3 1)))
           (gc-flip)
           (list ((record-accessor q 'a) r) ((record-accessor q 'b) r)
                 (map (lambda (f) ((record-accessor q f) s)) '(a b c))))))
(check "error objects survive collections"
       '(("boom" (1 (2 3))) ("car: not a pair" ()) #t)
       (let ((user (guard (e (#t e)) (error "boom" 1 (list 2 3))))
             (runtime (guard (e (#t e)) (car (list-tail (list 1) 1)))))
         (gc-flip)
         (list (list (error-object-message user) (error-object-irritants user))
               (list (error-object-message runtime)
                     (error-object-irritants runtime))
               (and (error-object? user) (error-object? runtime)))))
(check "bytevector ports survive collections" '(1 2 eof #u8(9 8))
       (let ((in (open-input-bytevector (bytevector 1 2)))
             (out (open-output-bytevector)))
         (write-u8 9 out)
         (gc-flip)
         (let* ((a (read-u8 in)) (b (read-u8 in)) (c (read-u8 in)))
           (write-u8 8 out)
           (list a b (if (eof-object? c) 'eof c) (get-output-bytevector out)))))
(check "promises and char-sets survive collections" '(3 #t #f)
       (let ((p (delay (+ 1 2))) (set (char-set "abc")))
         (gc-flip)
         (list (force p) (char-in-set? #\b set) (char-in-set? #\z set))))

;; Template keywords the use site rebinds become core aliases, through the
;; renamer, the matcher (case's else) and the interpreter's expansion of
;; procedure bodies; each of those allocates while holding forms.
(define-syntax gc-ck-or (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))
(define-syntax gc-ck-case (syntax-rules () ((_ x) (case x ((1) 'one) (else 0)))))
(define-syntax gc-ck-define (syntax-rules () ((_ n v) (define n v))))
(define (gc-ck-procedure n)
  (let ((if list) (let 0) (else #f) (define list))
    (gc-ck-define m (gc-ck-or #f n))
    (list m (gc-ck-case n)
          ((lambda (k) (cond ((assv k '((1 . one))) => cdr) (#t 'none))) n)
          (letrec ((loop (lambda (i acc) (cond ((= i 0) acc) (#t (loop (- i 1) (+ acc 1)))))))
            (loop 3 0)))))
(check "core aliases under keyword shadowing" '((5 0 none 3) (1 one one 3))
       (list (gc-ck-procedure 5) (gc-ck-procedure 1)))
;; The stdlib's macros had their keywords replaced by core aliases as it
;; loaded (macros_freeze_core_keywords); a template walk that lost a cell
;; there breaks every later use.
(check "stdlib do, case and when after loading" '(3 b 7)
       (list (do ((i 0 (+ i 1))) ((= i 3) i))
             (case 2 ((1) 'a) ((2) 'b) (else 'c))
             (when #t 7)))

;; The VM's variable caches hold cell indices that every collection moves;
;; each collection must drop them. Under stress one lands between nearly
;; every fill and its next use.
(define gcs-x 'global)
(define (gcs-make flag)
  (let ()
    (if flag (define gcs-x (list 'local flag)))
    (lambda () gcs-x)))
(check "cached lookups across activations and collections"
       '(global (local #t) global (local #t))
       (let* ((a (gcs-make #t)) (b (gcs-make #f)))
         (list (b) (a) (b) (a))))
(define (gcs-adder n) (lambda (y) (+ y n)))
(check "closures over many activations" '(11 12 13 14 15)
       (map (lambda (f) (f 10)) (map gcs-adder '(1 2 3 4 5))))
(define (gcs-sum l) (if (null? l) 0 (+ (car l) (gcs-sum (cdr l)))))
(check "assigning a global used through cached call sites" '(6 2)
       (let* ((saved +) (r1 (gcs-sum '(1 2 3))))
         (set! + -)
         (let ((r2 (gcs-sum '(1 2 3))))
           (set! + saved)
           (list r1 r2))))
;; Let bindings in stack slots live on the VM stack between the parameters
;; and the operands. A collection in the middle of a let body has to find and
;; forward them like parameters, and a re-entered continuation restores them.
(define (gc-ls-loop n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (let ((pair (cons i (* i i))) (tag (list 'x i)))
          (gc-flip)
          (let* ((sum (+ (car pair) (cdr pair))) (cell (list sum tag)))
            (loop (+ i 1) (cons cell acc)))))))
(check "let slots survive collections" '((0 (x 0)) (2 (x 1)) (6 (x 2)))
       (gc-ls-loop 3))
(define gc-ls-k #f)
(define (gc-ls-reenter)
  (let ((a (list 1 2)) (b (call/cc (lambda (k) (set! gc-ls-k k) 0))))
    (gc-flip)
    (cons b a)))
(check "re-entered let body after collections" '((0 1 2) (1 1 2) (2 1 2))
       (let ((n 0) (out '()))
         (let ((v (gc-ls-reenter)))
           (set! out (cons v out))
           (set! n (+ n 1))
           (if (< n 3) (gc-ls-k n))
           (reverse out))))
;; Each of these went through code that rooted a local inside a block and
;; kept allocating after the block ended, so a collection wrote through the
;; dead stack slot: a call the compiler cannot inline as a let, a body macro
;; with a custom ellipsis, free locals and a quasiquote, a macro-defining
;; macro, and a procedure whose body macro needs an alias of a local (the
;; expander's fallback walk).
(define (gc-scope-not-inlined)
  (guard (e (#t 'arity))
    ((lambda (a b) (list a b)) 1)))
(define (gc-scope-body-macro x)
  (define-syntax twice
    (syntax-rules ::: ()
      ((_ e :::) (list (list x e) ::: `(x ,x)))))
  (twice 1 2))
(define-syntax gc-scope-def-macro
  (syntax-rules ()
    ((_ name v)
     (define-syntax name
       (syntax-rules ::: () ((_ a :::) (list v a :::)))))))
(gc-scope-def-macro gc-scope-m 0)
(define (gc-scope-placeholder n)
  (let ((k n))
    (define-syntax get-k (syntax-rules () ((_) k)))
    (let ((k 'shadow))
      (lambda () (list k (get-k))))))
(check "block-scoped roots"
       '(arity ((5 1) (5 2) (x 5)) (0 1 2) (shadow 4) #t)
       (list (gc-scope-not-inlined)
             (gc-scope-body-macro 5)
             (gc-scope-m 1 2)
             ((gc-scope-placeholder 4))
             (equal? (char-set #\a #\b) (char-set #\b #\a))))

;; Unassigned letrec variables and internal definitions: the marker is a
;; permanent cell, so it must survive every collection as itself, and the
;; error raised on reading it must be an ordinary, catchable error object.
(define (gc-unassigned thunk)
  (guard (e ((error-object? e) (error-object-message e))) (thunk)))
(define (gc-unassigned-defs flag)
  (define (get) v)
  (define w (if flag (get) (make-list 50 'w)))
  (define v (make-list 50 'v))
  (list (length w) (length (get))))
(check "unassigned variables"
       '("Unassigned variable: y" "Unassigned variable: v" (50 50)
         "Unassigned variable: b" 5 (1 2 3))
       (list (gc-unassigned
              (lambda () (letrec ((x (list 1 2 y)) (y (make-list 9 0))) x)))
             (gc-unassigned (lambda () (gc-unassigned-defs #t)))
             (gc-unassigned-defs #f)
             (gc-unassigned
              (lambda () (letrec* ((a (lambda () b)) (c (a)) (b 1)) c)))
             (letrec ((x (begin (set! x (make-list 5 0)) (length x)))) x)
             (letrec ((a 1) (b (+ a 1)) (c (list a b 3))) c)))
;; Procedural transformers run Scheme code while a unit is being expanded or
;; compiled: the transformer allocates freely, and everything the expander
;; and compiler hold across the call must be rooted.
(define-syntax gc-er-swap!
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((a (cadr form)) (b (caddr form)))
       `(,(rename 'let) ((,(rename 'tmp) ,a))
          (,(rename 'set!) ,a ,b)
          (,(rename 'set!) ,b ,(rename 'tmp)))))))
(define-syntax gc-sc-push
  (sc-macro-transformer
   (lambda (exp env)
     (let ((item (make-syntactic-closure env '() (cadr exp)))
           (lst (make-syntactic-closure env '() (caddr exp))))
       `(set! ,lst (cons ,item ,lst))))))
(define (gc-procedural-macros n)
  (let ((tmp n) (other 0) (l '()) (cons list))
    (gc-er-swap! tmp other)
    (gc-sc-push tmp l)
    (list tmp other l)))
(check "procedural macros" '((0 7 (0)) (0 8 (0)))
       (list (gc-procedural-macros 7)
             (let-syntax ((m (er-macro-transformer
                              (lambda (f r c)
                                `(,(r 'gc-procedural-macros) ,(cadr f))))))
               (m 8))))

;; (command-line) is the script's own view of its invocation - its name and
;; arguments - without vesper's options. make test-gcstress runs this file as
;; `vesper [--interpreter] gc_stress_tests.scm`.
(check "command-line holds the script, not interpreter options" #t
       (let* ((line (command-line))
              (name (car line))
              (suffix "gc_stress_tests.scm"))
         (and (not (member "--interpreter" line))
              (>= (string-length name) (string-length suffix))
              (string=? suffix
                        (substring name (- (string-length name)
                                           (string-length suffix))
                                   (string-length name))))))

(newline)
(display "GC stress tests: ")
(display (if (= failures 0) "all passed" "FAILURES"))
(newline)
(if (> failures 0) (exit 1))
