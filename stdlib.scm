; R3RS Standard Library
; This file is loaded automatically on startup

;;; ============================================================================
;;; Let forms as macros
;;; ============================================================================

; letrec - recursive binding (must be defined first, uses only lambda)
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var val) ...) body ...)
     ((lambda (var ...)
        (set! var val) ...
        body ...)
      #f ...))))

; let - parallel binding (named let uses letrec defined above)
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))
    ; Named let
    ((let name ((var val) ...) body ...)
     (letrec ((name (lambda (var ...) body ...)))
       (name val ...)))))

; let* - sequential binding (uses lambda directly)
; Need explicit patterns for different binding counts due to ellipsis limitations
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (begin body ...))
    ((let* ((var val) . more-bindings) body ...)
     ((lambda (var)
        (let* more-bindings body ...))
      val))))

;;; ============================================================================
;;; Case and Do macros
;;; ============================================================================

; case - multi-way branch
(define-syntax case
  (syntax-rules (else)
    ((case key (else result ...))
     (begin result ...))
    ((case key ((atoms ...) result ...))
     (if (memv key '(atoms ...))
         (begin result ...)))
    ((case key ((atoms ...) result ...) clause ...)
     (if (memv key '(atoms ...))
         (begin result ...)
         (case key clause ...)))))

; do - iteration
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec ((loop
               (lambda (var ...)
                 (if test
                     (begin #f expr ...)
                     (begin
                       command ...
                       (loop (do-step var step ...) ...))))))
       (loop init ...)))
    ))

(define-syntax do-step
  (syntax-rules ()
    ((do-step var) var)
    ((do-step var step) step)))

;;; ============================================================================
;;; List accessors (compositions of car/cdr)
;;; ============================================================================

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;;; ============================================================================
;;; List utilities
;;; ============================================================================

(define (list-ref lst k)
  (if (= k 0)
      (car lst)
      (list-ref (cdr lst) (- k 1))))

(define (list-tail lst k)
  (if (= k 0)
      lst
      (list-tail (cdr lst) (- k 1))))

; Membership functions
(define (memq obj lst)
  (cond ((null? lst) #f)
        ((eq? obj (car lst)) lst)
        (else (memq obj (cdr lst)))))

(define (memv obj lst)
  (cond ((null? lst) #f)
        ((eqv? obj (car lst)) lst)
        (else (memv obj (cdr lst)))))

(define (member obj lst)
  (cond ((null? lst) #f)
        ((equal? obj (car lst)) lst)
        (else (member obj (cdr lst)))))

; Association list functions
(define (assq obj alist)
  (cond ((null? alist) #f)
        ((eq? obj (caar alist)) (car alist))
        (else (assq obj (cdr alist)))))

(define (assv obj alist)
  (cond ((null? alist) #f)
        ((eqv? obj (caar alist)) (car alist))
        (else (assv obj (cdr alist)))))

(define (assoc obj alist)
  (cond ((null? alist) #f)
        ((equal? obj (caar alist)) (car alist))
        (else (assoc obj (cdr alist)))))

;;; ============================================================================
;;; Numeric predicates
;;; ============================================================================

(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (odd? n) (= (remainder n 2) 1))
(define (even? n) (= (remainder n 2) 0))

(define (max x . rest)
  (if (null? rest)
      x
      (let ((m (apply max rest)))
        (if (> x m) x m))))

(define (min x . rest)
  (if (null? rest)
      x
      (let ((m (apply min rest)))
        (if (< x m) x m))))

(define (gcd . args)
  (define (gcd2 a b)
    (if (= b 0)
        (abs a)
        (gcd2 b (remainder a b))))
  (if (null? args)
      0
      (let loop ((result (car args)) (rest (cdr args)))
        (if (null? rest)
            (abs result)
            (loop (gcd2 result (car rest)) (cdr rest))))))

(define (lcm . args)
  (define (lcm2 a b)
    (if (or (= a 0) (= b 0))
        0
        (abs (quotient (* a b) (gcd a b)))))
  (if (null? args)
      1
      (let loop ((result (car args)) (rest (cdr args)))
        (if (null? rest)
            (abs result)
            (loop (lcm2 result (car rest)) (cdr rest))))))

;;; ============================================================================
;;; Higher-order functions
;;; ============================================================================

(define (map proc lst . lsts)
  (if (null? lsts)
      ; Single list case
      (if (null? lst)
          '()
          (cons (proc (car lst))
                (map proc (cdr lst))))
      ; Multiple lists case
      (if (null? lst)
          '()
          (cons (apply proc (car lst) (map car lsts))
                (apply map proc (cdr lst) (map cdr lsts))))))

(define (for-each proc lst . lsts)
  (if (null? lsts)
      ; Single list case
      (if (not (null? lst))
          (begin
            (proc (car lst))
            (for-each proc (cdr lst))))
      ; Multiple lists case
      (if (not (null? lst))
          (begin
            (apply proc (car lst) (map car lsts))
            (apply for-each proc (cdr lst) (map cdr lsts))))))

;;; ============================================================================
;;; Additional utilities
;;; ============================================================================

; not is already a primitive, but define it for completeness
; (define (not x) (if x #f #t))

; Force/delay (simple implementation)
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (let ((forced #f) (value #f))
       (lambda ()
         (if forced
             value
             (begin
               (set! value expr)
               (set! forced #t)
               value)))))))

(define (force promise)
  (promise))

;;; ============================================================================
;;; File I/O forms (R3RS)
;;; ============================================================================

(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (let ((result (proc port)))
      (close-input-port port)
      result)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (let ((result (proc port)))
      (close-output-port port)
      result)))

; with-input-from-file temporarily redirects current-input-port
; Note: This is a simplified implementation
(define (with-input-from-file filename thunk)
  (let ((port (open-input-file filename)))
    (let ((result (thunk)))
      (close-input-port port)
      result)))

; with-output-to-file temporarily redirects current-output-port
; Note: This is a simplified implementation
(define (with-output-to-file filename thunk)
  (let ((port (open-output-file filename)))
    (let ((result (thunk)))
      (close-output-port port)
      result)))

;;; ============================================================================
;;; String port convenience functions
;;; ============================================================================

; Call proc with a fresh output string port, return accumulated string
(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))

; Call proc with an input string port reading from str
(define (call-with-input-string str proc)
  (let ((port (open-input-string str)))
    (proc port)))

; Execute thunk with output captured to a string
(define-syntax with-output-to-string
  (syntax-rules ()
    ((with-output-to-string body ...)
     (let ((port (open-output-string)))
       (let ((write-to-port (lambda args
                              (if (null? args)
                                  (newline port)
                                  (display (car args) port)))))
         body ...)
       (get-output-string port)))))

; Execute thunk with input coming from a string
(define-syntax with-input-from-string
  (syntax-rules ()
    ((with-input-from-string str body ...)
     (let ((port (open-input-string str)))
       body ...))))

;;; ============================================================================
;;; List Processing Utilities (SRFI-1 style)
;;; ============================================================================

; filter - return list of elements satisfying predicate
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

; remove - return list of elements NOT satisfying predicate
(define (remove pred lst)
  (filter (lambda (x) (not (pred x))) lst))

; find - return first element satisfying predicate, or #f
(define (find pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find pred (cdr lst)))))

; any - return #t if any element satisfies predicate
(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))

; every - return #t if all elements satisfy predicate
(define (every pred lst)
  (cond ((null? lst) #t)
        ((not (pred (car lst))) #f)
        (else (every pred (cdr lst)))))

; count - count elements satisfying predicate
(define (count pred lst)
  (let loop ((lst lst) (n 0))
    (cond ((null? lst) n)
          ((pred (car lst)) (loop (cdr lst) (+ n 1)))
          (else (loop (cdr lst) n)))))

; fold-left (reduce) - left-associative fold
(define (fold proc init lst)
  (if (null? lst)
      init
      (fold proc (proc (car lst) init) (cdr lst))))

; fold-right - right-associative fold
(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (fold-right proc init (cdr lst)))))

; reduce - like fold but uses first element as initial value
(define (reduce proc lst)
  (if (null? lst)
      (error "reduce: empty list")
      (fold proc (car lst) (cdr lst))))

; take - return first n elements of list
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

; drop - return list without first n elements
(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

; partition - split list into two lists based on predicate
(define (partition pred lst)
  (let loop ((lst lst) (yes '()) (no '()))
    (cond ((null? lst) (list (reverse yes) (reverse no)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no))
          (else (loop (cdr lst) yes (cons (car lst) no))))))

; zip - combine corresponding elements of two lists
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

; flatten - flatten nested list structure
(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

; last - return last element of list
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

; iota - generate list of integers [0, n)
(define (iota n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons i acc)))))

; range - generate list of integers [start, end)
(define (range start end)
  (let loop ((i (- end 1)) (acc '()))
    (if (< i start)
        acc
        (loop (- i 1) (cons i acc)))))

;;; ============================================================================
;;; Additional SRFI-1 Constructors
;;; ============================================================================

; xcons - like cons but with reversed arguments
(define (xcons d a) (cons a d))

; cons* - like list but last arg is tail
(define (cons* first . rest)
  (if (null? rest)
      first
      (cons first (apply cons* rest))))

; make-list - create list of n elements, optionally filled with fill
(define (make-list n . fill)
  (let ((f (if (null? fill) #f (car fill))))
    (let loop ((n n) (acc '()))
      (if (<= n 0)
          acc
          (loop (- n 1) (cons f acc))))))

; list-tabulate - create list by calling init-proc on indices 0..n-1
(define (list-tabulate n init-proc)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons (init-proc i) acc)))))

; list-copy - shallow copy of list
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

;;; ============================================================================
;;; Additional SRFI-1 Predicates
;;; ============================================================================

; not-pair? - true if x is not a pair
(define (not-pair? x) (not (pair? x)))

; null-list? - like null? but signals error on improper list
(define (null-list? lst)
  (cond ((null? lst) #t)
        ((pair? lst) #f)
        (else (error "null-list?: not a proper list"))))

; proper-list? - true if x is a proper list (finite, nil-terminated)
; Uses tortoise-and-hare algorithm for cycle detection
(define (proper-list? x)
  (let loop ((fast x) (slow x) (first? #t))
    (cond ((null? fast) #t)
          ((not (pair? fast)) #f)
          ((null? (cdr fast)) #t)
          ((not (pair? (cdr fast))) #f)
          ((and (not first?) (eq? fast slow)) #f)  ; cycle detected
          (else (loop (cddr fast) (cdr slow) #f)))))

; list? - alias for proper-list?
(define list? proper-list?)

; dotted-list? - true if x is a finite, non-nil-terminated list
(define (dotted-list? x)
  (let loop ((fast x) (slow x) (first? #t))
    (cond ((null? fast) #f)
          ((not (pair? fast)) #t)
          ((null? (cdr fast)) #f)
          ((not (pair? (cdr fast))) #t)
          ((and (not first?) (eq? fast slow)) #f)  ; cycle - not dotted
          (else (loop (cddr fast) (cdr slow) #f)))))

; list= - compare lists element-wise using elt=
(define (list= elt= . lists)
  (or (null? lists)
      (null? (cdr lists))
      (let loop ((lists lists))
        (or (null? (cdr lists))
            (let ((a (car lists)) (b (cadr lists)))
              (and (let cmp ((a a) (b b))
                     (cond ((null? a) (null? b))
                           ((null? b) #f)
                           ((elt= (car a) (car b)) (cmp (cdr a) (cdr b)))
                           (else #f)))
                   (loop (cdr lists))))))))

;;; ============================================================================
;;; Additional SRFI-1 Selectors
;;; ============================================================================

; first through tenth - positional accessors
(define (first x) (car x))
(define (second x) (cadr x))
(define (third x) (caddr x))
(define (fourth x) (cadddr x))
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))
(define (ninth x) (car (cddddr (cddddr x))))
(define (tenth x) (cadr (cddddr (cddddr x))))

; car+cdr - return both car and cdr as multiple values
(define (car+cdr pair)
  (values (car pair) (cdr pair)))

; take-right - return last n elements
(define (take-right lst n)
  (let ((len (length lst)))
    (drop (- len n) lst)))

; drop-right - return all but last n elements
(define (drop-right lst n)
  (let ((len (length lst)))
    (take (- len n) lst)))

; split-at - split list at index, return two lists
(define (split-at lst n)
  (values (take n lst) (drop n lst)))

; last-pair - return last pair of list
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;;; ============================================================================
;;; Additional SRFI-1 Miscellaneous
;;; ============================================================================

; concatenate - append all lists in list-of-lists
(define (concatenate lists)
  (apply append lists))

; append-reverse - (append (reverse rev-head) tail)
(define (append-reverse rev-head tail)
  (if (null? rev-head)
      tail
      (append-reverse (cdr rev-head) (cons (car rev-head) tail))))

; unzip1 - extract first elements from list of lists
(define (unzip1 lists)
  (map car lists))

; unzip2 - extract first two elements from list of lists
(define (unzip2 lists)
  (values (map car lists) (map cadr lists)))

; unzip3 - extract first three elements
(define (unzip3 lists)
  (values (map car lists) (map cadr lists) (map caddr lists)))

; unzip4 - extract first four elements
(define (unzip4 lists)
  (values (map car lists) (map cadr lists) (map caddr lists) (map cadddr lists)))

; unzip5 - extract first five elements
(define (unzip5 lists)
  (values (map car lists) (map cadr lists) (map caddr lists)
          (map cadddr lists) (map fifth lists)))

;;; ============================================================================
;;; Additional SRFI-1 Fold/Unfold/Map
;;; ============================================================================

; pair-fold - like fold but proc receives pairs, not elements
(define (pair-fold proc init lst)
  (if (null? lst)
      init
      (let ((tail (cdr lst)))
        (pair-fold proc (proc lst init) tail))))

; pair-fold-right - like fold-right but proc receives pairs
(define (pair-fold-right proc init lst)
  (if (null? lst)
      init
      (proc lst (pair-fold-right proc init (cdr lst)))))

; reduce-right - like reduce but right-associative
(define (reduce-right proc init lst)
  (if (null? lst)
      init
      (let loop ((lst lst))
        (if (null? (cdr lst))
            (car lst)
            (proc (car lst) (loop (cdr lst)))))))

; unfold - generate list from seed using p, f, g
(define (unfold p f g seed . maybe-tail-gen)
  (let ((tail-gen (if (null? maybe-tail-gen) (lambda (x) '()) (car maybe-tail-gen))))
    (let loop ((seed seed))
      (if (p seed)
          (tail-gen seed)
          (cons (f seed) (loop (g seed)))))))

; unfold-right - like unfold but builds list right-to-left
(define (unfold-right p f g seed . maybe-tail)
  (let ((tail (if (null? maybe-tail) '() (car maybe-tail))))
    (let loop ((seed seed) (acc tail))
      (if (p seed)
          acc
          (loop (g seed) (cons (f seed) acc))))))

; append-map - map then append results
(define (append-map proc lst . lsts)
  (apply append (apply map proc lst lsts)))

; filter-map - map and filter in one pass
(define (filter-map proc lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (let ((result (proc (car lst))))
          (loop (cdr lst) (if result (cons result acc) acc))))))

; pair-for-each - like for-each but proc receives pairs
(define (pair-for-each proc lst)
  (if (not (null? lst))
      (begin
        (proc lst)
        (pair-for-each proc (cdr lst)))))

;;; ============================================================================
;;; Additional SRFI-1 Searching
;;; ============================================================================

; find-tail - return tail of list starting at first match
(define (find-tail pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) lst)
        (else (find-tail pred (cdr lst)))))

; list-index - return index of first element satisfying pred
(define (list-index pred lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
          ((pred (car lst)) i)
          (else (loop (cdr lst) (+ i 1))))))

; take-while - return longest prefix satisfying pred
(define (take-while pred lst)
  (if (or (null? lst) (not (pred (car lst))))
      '()
      (cons (car lst) (take-while pred (cdr lst)))))

; drop-while - drop longest prefix satisfying pred
(define (drop-while pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (drop-while pred (cdr lst)))
        (else lst)))

; span - split at first element not satisfying pred
(define (span pred lst)
  (values (take-while pred lst) (drop-while pred lst)))

; break - split at first element satisfying pred
(define (break pred lst)
  (span (lambda (x) (not (pred x))) lst))

;;; ============================================================================
;;; Additional SRFI-1 Deletion
;;; ============================================================================

; delete - remove all elements equal to x
(define (delete x lst . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (filter (lambda (y) (not (eq x y))) lst)))

; delete-duplicates - remove duplicate elements
(define (delete-duplicates lst . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (let loop ((lst lst) (seen '()))
      (cond ((null? lst) (reverse seen))
            ((find (lambda (y) (eq (car lst) y)) seen)
             (loop (cdr lst) seen))
            (else (loop (cdr lst) (cons (car lst) seen)))))))

;;; ============================================================================
;;; Additional SRFI-1 Association Lists
;;; ============================================================================

; alist-cons - add entry to front of alist
(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

; alist-copy - shallow copy of alist
(define (alist-copy alist)
  (map (lambda (pair) (cons (car pair) (cdr pair))) alist))

; alist-delete - remove entries matching key
(define (alist-delete key alist . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (filter (lambda (pair) (not (eq key (car pair)))) alist)))

;;; ============================================================================
;;; SRFI-1 Linear Update (Mutation) Procedures
;;; ============================================================================

; take! - destructive version of take
(define (take! lst n)
  (cond ((<= n 0) '())
        ((null? lst) '())
        (else
         (let ((tail (list-tail lst (- n 1))))
           (if (pair? tail)
               (set-cdr! tail '()))
           lst))))

; drop-right! - destructive version of drop-right
(define (drop-right! lst n)
  (let ((len (length lst)))
    (if (<= (- len n) 0)
        '()
        (take! lst (- len n)))))

; split-at! - destructive version of split-at
(define (split-at! lst n)
  (if (<= n 0)
      (values '() lst)
      (let ((tail (drop lst (- n 1))))
        (let ((rest (cdr tail)))
          (set-cdr! tail '())
          (values lst rest)))))

; append! - destructive append
(define (append! . lists)
  (if (null? lists)
      '()
      (let loop ((result '()) (lists lists))
        (cond ((null? lists) result)
              ((null? (car lists)) (loop result (cdr lists)))
              ((null? result) (loop (car lists) (cdr lists)))
              (else
               (set-cdr! (last-pair result) (car lists))
               (loop result (cdr lists)))))))

; concatenate! - destructive concatenate
(define (concatenate! list-of-lists)
  (apply append! list-of-lists))

; reverse! - destructive reverse
(define (reverse! lst)
  (let loop ((prev '()) (curr lst))
    (if (null? curr)
        prev
        (let ((next (cdr curr)))
          (set-cdr! curr prev)
          (loop curr next)))))

; append-reverse! - destructive append-reverse
(define (append-reverse! rev-head tail)
  (let loop ((curr rev-head) (acc tail))
    (if (null? curr)
        acc
        (let ((next (cdr curr)))
          (set-cdr! curr acc)
          (loop next curr)))))

; map! - destructive map (mutates first list)
(define (map! proc lst . lsts)
  (if (null? lsts)
      ; Single list case
      (let loop ((pair lst))
        (if (null? pair)
            lst
            (begin
              (set-car! pair (proc (car pair)))
              (loop (cdr pair)))))
      ; Multiple lists case
      (let loop ((pair lst) (others lsts))
        (if (or (null? pair) (any null? others))
            lst
            (begin
              (set-car! pair (apply proc (car pair) (map car others)))
              (loop (cdr pair) (map cdr others)))))))

; filter! - destructive filter
(define (filter! pred lst)
  ; Skip leading non-matching elements to find new head
  (let find-head ((lst lst))
    (cond ((null? lst) '())
          ((pred (car lst))
           ; Found head, now filter rest in place
           (let loop ((prev lst) (curr (cdr lst)))
             (cond ((null? curr) lst)
                   ((pred (car curr))
                    (loop curr (cdr curr)))
                   (else
                    (set-cdr! prev (cdr curr))
                    (loop prev (cdr curr)))))
           lst)
          (else (find-head (cdr lst))))))

; remove! - destructive remove
(define (remove! pred lst)
  (filter! (lambda (x) (not (pred x))) lst))

; partition! - destructive partition
(define (partition! pred lst)
  (let ((yes (filter! pred (list-copy lst)))
        (no (remove! pred lst)))
    (values yes no)))

; take-while! - destructive take-while
(define (take-while! pred lst)
  (if (or (null? lst) (not (pred (car lst))))
      '()
      (let loop ((prev lst) (curr (cdr lst)))
        (cond ((null? curr) lst)
              ((pred (car curr)) (loop curr (cdr curr)))
              (else
               (set-cdr! prev '())
               lst)))))

; span! - destructive span
(define (span! pred lst)
  (if (or (null? lst) (not (pred (car lst))))
      (values '() lst)
      (let loop ((prev lst) (curr (cdr lst)))
        (cond ((null? curr) (values lst '()))
              ((pred (car curr)) (loop curr (cdr curr)))
              (else
               (set-cdr! prev '())
               (values lst curr))))))

; break! - destructive break
(define (break! pred lst)
  (span! (lambda (x) (not (pred x))) lst))

; delete! - destructive delete
(define (delete! x lst . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (filter! (lambda (y) (not (eq x y))) lst)))

; delete-duplicates! - destructive delete-duplicates
(define (delete-duplicates! lst . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (let loop ((curr lst))
      (if (null? curr)
          lst
          (begin
            (set-cdr! curr (delete! (car curr) (cdr curr) eq))
            (loop (cdr curr)))))))

; alist-delete! - destructive alist-delete
(define (alist-delete! key alist . maybe-eq)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (filter! (lambda (pair) (not (eq key (car pair)))) alist)))

