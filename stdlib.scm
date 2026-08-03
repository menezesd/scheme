; R3RS Standard Library
; This file is loaded automatically on startup

;;; ============================================================================
;;; Let forms as macros
;;; ============================================================================

; Note: letrec is built-in with proper call/cc semantics
; (continuation restoration when reinvoking captured continuations)

; let - parallel binding (named let uses built-in letrec)
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))
    ; Named let - evaluate inits in outer env, then bind name
    ((let name ((var val) ...) body ...)
     ((lambda (var ...)
        (letrec ((name (lambda (var ...) body ...)))
          (name var ...)))
      val ...))))

; let* - sequential binding (uses lambda directly)
; Need explicit patterns for different binding counts due to ellipsis limitations
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))  ; Use let for proper scoping of internal defines
    ((let* ((var val) . more-bindings) body ...)
     ((lambda (var)
        (let* more-bindings body ...))
      val))))

;;; ============================================================================
;;; MIT Scheme compatibility functions
;;; ============================================================================

;; string-search-forward: find first occurrence of pattern in text
;; Returns index or #f
(define (string-search-forward pattern text . opt-start)
  (let ((start (if (null? opt-start) 0 (car opt-start)))
        (plen (string-length pattern))
        (tlen (string-length text)))
    (let loop ((i start))
      (cond ((> (+ i plen) tlen) #f)
            ((string=? pattern (substring text i (+ i plen))) i)
            (else (loop (+ i 1)))))))

;; string-search-backward: find last occurrence of pattern in text
(define (string-search-backward pattern text . opt-end)
  (let ((plen (string-length pattern))
        (end (if (null? opt-end) (string-length text) (car opt-end))))
    (let loop ((i (- end plen)))
      (cond ((< i 0) #f)
            ((string=? pattern (substring text i (+ i plen))) i)
            (else (loop (- i 1)))))))

;;; ============================================================================
;;; Bitwise helpers (MIT Scheme compatibility)
;;; ============================================================================

(define (bit-set? bit-num val)
  (not (= 0 (bitwise-and val (arithmetic-shift 1 bit-num)))))

;; MIT Scheme compatibility aliases
(define random random-integer)

;; R7RS binary I/O
(define (write-u8 byte . opt-port)
  (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
    (cond
      ((bytevector-output-port-open? port)
       (vector-set! port 1 (cons (bytevector byte) (vector-ref port 1))))
      ((bytevector-output-port? port)
       (error "write-u8: port is closed"))
      (else
       (write-char (integer->char byte) port)))))

(define (read-u8 . opt-port)
  (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
    (cond
      ((bytevector-input-port-open? port)
       (let* ((bv (vector-ref port 1))
              (pos (vector-ref port 2)))
         (if (>= pos (bytevector-length bv))
             (eof-object)
             (begin
               (vector-set! port 2 (+ pos 1))
               (bytevector-u8-ref bv pos)))))
      ((bytevector-input-port? port)
       (error "read-u8: port is closed"))
      (else
       (let ((ch (read-char port)))
         (if (eof-object? ch) ch (char->integer ch)))))))

;;; ============================================================================
;;; Binary I/O helpers
;;; ============================================================================

(define (call-with-binary-input-file filename proc)
  (let ((port (open-binary-input-file filename)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close-input-port port)))))

;;; ============================================================================
;;; Bytevector ports (in-memory I/O using bytevectors)
;;; ============================================================================

;; Bytevector output port: vector #(tag chunks)
(define (open-output-bytevector)
  (vector 'bvout '()))

(define (bytevector-output-port? x)
  (and (vector? x) (> (vector-length x) 0)
       (or (eq? (vector-ref x 0) 'bvout)
           (eq? (vector-ref x 0) 'bvout-closed))))

(define (bytevector-output-port-open? x)
  (and (bytevector-output-port? x) (eq? (vector-ref x 0) 'bvout)))

(define (get-output-bytevector port)
  (if (bytevector-output-port-open? port)
      (apply bytevector-append (reverse (vector-ref port 1)))
      (error "get-output-bytevector: port is closed")))

;; Bytevector input port: vector #(tag bv pos)
(define (open-input-bytevector bv)
  (vector 'bvin bv 0))

(define (bytevector-input-port? x)
  (and (vector? x) (> (vector-length x) 0)
       (or (eq? (vector-ref x 0) 'bvin)
           (eq? (vector-ref x 0) 'bvin-closed))))

(define (bytevector-input-port-open? x)
  (and (bytevector-input-port? x) (eq? (vector-ref x 0) 'bvin)))

;; write-bytevector: write all or part of a bytevector to a port
(define primitive-write-bytevector write-bytevector)

(define (write-bytevector bv . args)
  (let* ((port (if (pair? args) (car args) (current-output-port)))
         (rest (if (pair? args) (cdr args) '()))
         (start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (bytevector-length bv))))
    (cond
      ((bytevector-output-port-open? port)
       (vector-set! port 1 (cons (bytevector-copy bv start end) (vector-ref port 1))))
      ((bytevector-output-port? port)
       (error "write-bytevector: port is closed"))
      (else
       (primitive-write-bytevector (bytevector-copy bv start end) port)))))

;; Wrap read-bytevector to support bytevector input ports
(let ((prim-read-bytevector read-bytevector))
  (set! read-bytevector
    (lambda (k . args)
     (let ((port (if (pair? args) (car args) (current-input-port))))
      (cond
        ((bytevector-input-port-open? port)
         (let* ((bv (vector-ref port 1))
                (pos (vector-ref port 2))
                (available (- (bytevector-length bv) pos)))
           (if (<= available 0)
               (eof-object)
               (let* ((n (min k available))
                      (result (bytevector-copy bv pos (+ pos n))))
                 (vector-set! port 2 (+ pos n))
                 result))))
        ((bytevector-input-port? port)
         (error "read-bytevector: port is closed"))
        (else
         (prim-read-bytevector k port)))))))

;; peek-u8/u8-ready? are C primitives that only understand real ports;
;; wrap them the same way read-u8/read-bytevector are wrapped above so
;; they also accept the vector-based bytevector input ports.
(let ((primitive-peek-u8 peek-u8)
      (primitive-u8-ready? u8-ready?))
  (set! peek-u8
    (lambda opt-port
      (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
        (cond
          ((bytevector-input-port-open? port)
           (let* ((bv (vector-ref port 1))
                  (pos (vector-ref port 2)))
             (if (>= pos (bytevector-length bv))
                 (eof-object)
                 (bytevector-u8-ref bv pos))))
          ((bytevector-input-port? port)
           (error "peek-u8: port is closed"))
          (else
           (apply primitive-peek-u8 opt-port))))))
  (set! u8-ready?
    (lambda opt-port
      (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
        (cond
          ((bytevector-input-port-open? port) #t)
          ((bytevector-input-port? port)
           (error "u8-ready?: port is closed"))
          (else
           (apply primitive-u8-ready? opt-port)))))))

(let ((primitive-input-port? input-port?)
      (primitive-output-port? output-port?)
      (primitive-input-port-open? input-port-open?)
      (primitive-output-port-open? output-port-open?)
      (primitive-textual-port? textual-port?)
      (primitive-binary-port? binary-port?)
      (primitive-close-input-port close-input-port)
      (primitive-close-output-port close-output-port))
  (set! input-port?
    (lambda (obj)
      (or (bytevector-input-port? obj)
          (primitive-input-port? obj))))
  (set! output-port?
    (lambda (obj)
      (or (bytevector-output-port? obj)
          (primitive-output-port? obj))))
  (set! input-port-open?
    (lambda (obj)
      (if (bytevector-input-port? obj)
          (bytevector-input-port-open? obj)
          (primitive-input-port-open? obj))))
  (set! output-port-open?
    (lambda (obj)
      (if (bytevector-output-port? obj)
          (bytevector-output-port-open? obj)
          (primitive-output-port-open? obj))))
  (set! textual-port?
    (lambda (obj)
      (and (not (or (bytevector-input-port? obj)
                    (bytevector-output-port? obj)))
           (primitive-textual-port? obj))))
  (set! binary-port?
    (lambda (obj)
      (or (bytevector-input-port? obj)
          (bytevector-output-port? obj)
          (primitive-binary-port? obj))))
  (set! close-input-port
    (lambda (port)
      (if (bytevector-input-port-open? port)
          (vector-set! port 0 'bvin-closed)
          (primitive-close-input-port port))))
  (set! close-output-port
    (lambda (port)
      (if (bytevector-output-port-open? port)
          (vector-set! port 0 'bvout-closed)
          (primitive-close-output-port port)))))

;;; ============================================================================
;;; When/Unless macros (R7RS)
;;; ============================================================================

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))

;;; ============================================================================
;;; Guard (R7RS exception handling)
;;; ============================================================================

(define primitive-error error)
(define *error-object-tag* (list 'error-object))

(define (make-error-object kind message irritants)
  (vector *error-object-tag* kind message irritants))

(define (error-object? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) *error-object-tag*)))

(define (error-object-message obj)
  (if (error-object? obj)
      (vector-ref obj 2)
      (primitive-error "error-object-message: not an error object")))

(define (error-object-irritants obj)
  (if (error-object? obj)
      (vector-ref obj 3)
      (primitive-error "error-object-irritants: not an error object")))

(define (read-error? obj)
  (and (error-object? obj) (eq? (vector-ref obj 1) 'read)))

(define (file-error? obj)
  (and (error-object? obj) (eq? (vector-ref obj 1) 'file)))

;; Simple exception system using continuations
(define *current-exception-handler*
  (lambda (exn)
    (display "Unhandled exception: ")
    (display exn)
    (newline)
    (exit 1)))

;; Per R7RS 6.11, a handler invoked by raise/raise-continuable must run
;; with the PREVIOUS handler in effect (not itself), so that a handler
;; which re-raises (e.g. a guard clause that falls through) propagates to
;; the enclosing handler instead of calling itself forever. We install a
;; wrapper that swaps back to the previous handler for the duration of the
;; user handler's call, restoring itself afterward if the handler returns
;; (the raise-continuable case).
(define (with-exception-handler handler thunk)
  (let ((old-handler *current-exception-handler*))
    (letrec ((wrapped-handler
               (lambda (obj)
                 (dynamic-wind
                   (lambda () (set! *current-exception-handler* old-handler))
                   (lambda () (handler obj))
                   (lambda () (set! *current-exception-handler* wrapped-handler))))))
      (dynamic-wind
        (lambda () (set! *current-exception-handler* wrapped-handler))
        thunk
        (lambda () (set! *current-exception-handler* old-handler))))))

(define (raise obj)
  (*current-exception-handler* obj)
  (primitive-error "handler returned from non-continuable exception" obj))

(define (raise-continuable obj)
  (*current-exception-handler* obj))

(define (error message . irritants)
  (raise (make-error-object 'error message irritants)))

(define (syntax-error message . irritants)
  (raise (make-error-object 'read message irritants)))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-exit)
         (with-exception-handler
           (lambda (var)
             (guard-exit
               (guard-aux var clause ...)))
           (lambda ()
             body ...)))))))

(define-syntax guard-aux
  (syntax-rules (else)
    ((guard-aux var (else result ...))
     (begin result ...))
    ((guard-aux var (test result ...))
     (if test (begin result ...) (raise var)))
    ((guard-aux var (test result ...) clause ...)
     (if test (begin result ...) (guard-aux var clause ...)))))

;;; ============================================================================
;;; Case and Do macros
;;; ============================================================================

; case - multi-way branch
(define-syntax %case-dispatch
  (syntax-rules (else)
    ((%case-dispatch key (else result ...))
     (begin result ...))
    ((%case-dispatch key ((atoms ...) result ...))
     (if (memv key '(atoms ...))
         (begin result ...)))
    ((%case-dispatch key ((atoms ...) result ...) clause ...)
     (if (memv key '(atoms ...))
         (begin result ...)
         (%case-dispatch key clause ...)))))

(define-syntax case
  (syntax-rules ()
    ((case key clause ...)
     (let ((case-key key))
       (%case-dispatch case-key clause ...)))))

; do - iteration (uses nested ellipsis)
; do - iteration (uses nested ellipsis for any number of variables)
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

(define (%require-proper-list who lst)
  (if (list? lst)
      lst
      (error (string-append who ": expected proper list"))))

(define (%require-nonnegative-integer who k)
  (if (and (integer? k) (>= k 0))
      k
      (error (string-append who ": expected nonnegative integer"))))

; Like %require-proper-list, but allows a non-nil final cdr (an improper/
; dotted list), as several R7RS/SRFI-1 procedures explicitly permit.
; Still rejects circular lists (Floyd's cycle detection) so callers can't
; hang walking one.
(define (%require-finite-list who lst)
  (let loop ((slow lst) (fast lst))
    (cond ((not (pair? fast)) lst)
          ((not (pair? (cdr fast))) lst)
          (else
           (let ((fast2 (cddr fast)) (slow2 (cdr slow)))
             (if (eq? fast2 slow2)
                 (error (string-append who ": circular list"))
                 (loop slow2 fast2)))))))

; Like %require-proper-lists, but the LAST element of `lists` may be any
; object (R7RS `append`'s convention: every argument but the last must be
; a list, and the last is used as-is, becoming the final tail).
(define (%require-proper-lists-but-last who lists)
  (if (pair? lists)
      (let loop ((lists lists))
        (if (pair? (cdr lists))
            (begin
              (%require-proper-list who (car lists))
              (loop (cdr lists)))
            lists))
      lists))

(define (%require-proper-lists who lists)
  (%require-proper-list who lists)
  (let loop ((lists lists))
    (if (not (null? lists))
        (begin
          (%require-proper-list who (car lists))
          (loop (cdr lists)))))
  lists)

(define (list-ref lst k)
  (%require-nonnegative-integer "list-ref" k)
  (let loop ((lst lst) (k k))
    (cond ((not (pair? lst)) (error "list-ref: index out of bounds"))
          ((= k 0) (car lst))
          (else (loop (cdr lst) (- k 1))))))

(define (list-tail lst k)
  (%require-nonnegative-integer "list-tail" k)
  (let loop ((lst lst) (k k))
    (cond ((= k 0) lst)
          ((not (pair? lst)) (error "list-tail: index out of bounds"))
          (else (loop (cdr lst) (- k 1))))))

; Membership functions
(define (memq obj lst)
  (%require-proper-list "memq" lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((eq? obj (car lst)) lst)
          (else (loop (cdr lst))))))

(define (memv obj lst)
  (%require-proper-list "memv" lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((eqv? obj (car lst)) lst)
          (else (loop (cdr lst))))))

(define (member obj lst . maybe-compare)
  (%require-proper-list "member" lst)
  (let ((compare (if (null? maybe-compare) equal? (car maybe-compare))))
    (let loop ((lst lst))
      (cond ((null? lst) #f)
            ((compare obj (car lst)) lst)
            (else (loop (cdr lst)))))))

; Association list functions
(define (assq obj alist)
  (%require-proper-list "assq" alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
          ((eq? obj (caar alist)) (car alist))
          (else (loop (cdr alist))))))

(define (assv obj alist)
  (%require-proper-list "assv" alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
          ((eqv? obj (caar alist)) (car alist))
          (else (loop (cdr alist))))))

(define (assoc obj alist . maybe-compare)
  (%require-proper-list "assoc" alist)
  (let ((compare (if (null? maybe-compare) equal? (car maybe-compare))))
    (let loop ((alist alist))
      (cond ((null? alist) #f)
            ((compare obj (caar alist)) (car alist))
            (else (loop (cdr alist)))))))

;;; ============================================================================
;;; Numeric predicates
;;; ============================================================================

(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (odd? n) (not (= (remainder n 2) 0)))
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
  (%require-proper-list "map" lst)
  (%require-proper-lists "map" lsts)
  (if (null? lsts)
      ; Single list case - tail-recursive, builds in order via set-cdr!
      (let ((head (cons '() '())))
        (let loop ((lst lst) (tail head))
          (if (null? lst)
              (cdr head)
              (let ((new-cell (cons (proc (car lst)) '())))
                (set-cdr! tail new-cell)
                (loop (cdr lst) new-cell)))))
      ; Multiple lists case - tail-recursive, builds in order
      (let ((head (cons '() '())))
        (let loop ((lst lst) (lsts lsts) (tail head))
          (if (or (null? lst) (any null? lsts))
              (cdr head)
              (let ((new-cell (cons (apply proc (car lst) (map car lsts)) '())))
                (set-cdr! tail new-cell)
                (loop (cdr lst) (map cdr lsts) new-cell)))))))

(define (for-each proc lst . lsts)
  (%require-proper-list "for-each" lst)
  (%require-proper-lists "for-each" lsts)
  (if (null? lsts)
      ; Single list case
      (if (not (null? lst))
          (begin
            (proc (car lst))
            (for-each proc (cdr lst))))
      ; Multiple lists case
      (if (not (or (null? lst) (any null? lsts)))
          (begin
            (apply proc (car lst) (map car lsts))
            (apply for-each proc (cdr lst) (map cdr lsts))))))

;;; ============================================================================
;;; Additional utilities
;;; ============================================================================

; not is already a primitive, but define it for completeness
; (define (not x) (if x #f #t))

(define *promise-tag* (list 'promise))

(define (%make-lazy-promise thunk)
  (vector *promise-tag* #f thunk))

(define (make-promise obj)
  (if (promise? obj)
      obj
      (vector *promise-tag* #t obj)))

(define (promise? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) *promise-tag*)))

; Force/delay
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (%make-lazy-promise (lambda () expr)))))

(define (force promise)
  (if (not (promise? promise))
      (error "force: not a promise")
      ; Iterative, not recursive: a chain of delay-force promises (the
      ; standard lazy-stream idiom) must force in constant stack space per
      ; R7RS 4.2.5. Recursing via (force result) here would grow the stack
      ; by one frame per link in the chain.
      (let loop ((p promise))
        (if (vector-ref p 1)
            (vector-ref p 2)
            (let ((result ((vector-ref p 2))))
              (if (promise? result)
                  (begin
                    (vector-set! p 1 (vector-ref result 1))
                    (vector-set! p 2 (vector-ref result 2))
                    (loop p))
                  (begin
                    (vector-set! p 1 #t)
                    (vector-set! p 2 result)
                    result)))))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expr)
     (%make-lazy-promise (lambda () expr)))))

;;; ============================================================================
;;; File I/O forms (R3RS)
;;; ============================================================================

(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close-input-port port)))))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close-output-port port)))))

; with-input-from-file temporarily redirects current-input-port
(define (with-input-from-file filename thunk)
  (let* ((new-port (open-input-file filename))
         (old-port (current-input-port)))
    (dynamic-wind
      (lambda () (set-current-input-port! new-port))
      thunk
      (lambda ()
        (set-current-input-port! old-port)
        (close-input-port new-port)))))

; with-output-to-file temporarily redirects current-output-port
(define (with-output-to-file filename thunk)
  (let* ((new-port (open-output-file filename))
         (old-port (current-output-port)))
    (dynamic-wind
      (lambda () (set-current-output-port! new-port))
      thunk
      (lambda ()
        (set-current-output-port! old-port)
        (close-output-port new-port)))))

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

(define (%with-output-to-string thunk)
  (let ((port (open-output-string))
        (old-port (current-output-port)))
    (dynamic-wind
      (lambda () (set-current-output-port! port))
      thunk
      (lambda () (set-current-output-port! old-port)))
    (get-output-string port)))

(define (%with-input-from-string str thunk)
  (let ((port (open-input-string str))
        (old-port (current-input-port)))
    (dynamic-wind
      (lambda () (set-current-input-port! port))
      thunk
      (lambda () (set-current-input-port! old-port)))))

; Execute thunk with output captured to a string
(define-syntax with-output-to-string
  (syntax-rules ()
    ((with-output-to-string body ...)
     (%with-output-to-string (lambda () body ...)))))

; Execute thunk with input coming from a string
(define-syntax with-input-from-string
  (syntax-rules ()
    ((with-input-from-string str body ...)
     (%with-input-from-string str (lambda () body ...)))))

;;; ============================================================================
;;; List Processing Utilities (SRFI-1 style)
;;; ============================================================================

; filter - return list of elements satisfying predicate (tail-recursive, in-order)
(define (filter pred lst)
  (%require-proper-list "filter" lst)
  (let ((head (cons '() '()))) ; dummy head cell
    (let loop ((lst lst) (tail head))
      (cond ((null? lst) (cdr head))
            ((pred (car lst))
             (let ((new-cell (cons (car lst) '())))
               (set-cdr! tail new-cell)
               (loop (cdr lst) new-cell)))
            (else (loop (cdr lst) tail))))))

; remove - return list of elements NOT satisfying predicate
(define (remove pred lst)
  (filter (lambda (x) (not (pred x))) lst))

; find - return first element satisfying predicate, or #f
(define (find pred lst)
  (%require-proper-list "find" lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((pred (car lst)) (car lst))
          (else (loop (cdr lst))))))

; any - return #t if any element satisfies predicate
; any - return the predicate's true value for the first element(s) that
; satisfy it (not a bare #t), stopping at the shortest of any number of
; lists; #f if none satisfy (SRFI-1)
(define (any pred lst . more-lists)
  (if (null? more-lists)
      (begin
        (%require-proper-list "any" lst)
        (let loop ((lst lst))
          (cond ((null? lst) #f)
                ((pred (car lst)))
                (else (loop (cdr lst))))))
      (let loop ((lsts (cons lst more-lists)))
        (if (any-null? lsts)
            #f
            (or (apply pred (map car lsts))
                (loop (map cdr lsts)))))))

; every - return the predicate's value for the last element(s) (not a bare
; #t), stopping at the shortest of any number of lists; #t if all satisfy
; (or any list is empty from the start) (SRFI-1)
(define (every pred lst . more-lists)
  (if (null? more-lists)
      (begin
        (%require-proper-list "every" lst)
        (let loop ((lst lst) (last-result #t))
          (cond ((null? lst) last-result)
                ((pred (car lst)) => (lambda (r) (loop (cdr lst) r)))
                (else #f))))
      (let loop ((lsts (cons lst more-lists)) (last-result #t))
        (if (any-null? lsts)
            last-result
            (let ((r (apply pred (map car lsts))))
              (if r (loop (map cdr lsts) r) #f))))))

(define (any-null? lsts)
  (if (null? lsts) #f (or (null? (car lsts)) (any-null? (cdr lsts)))))

; count - count elements satisfying predicate
(define (count pred lst)
  (%require-proper-list "count" lst)
  (let loop ((lst lst) (n 0))
    (cond ((null? lst) n)
          ((pred (car lst)) (loop (cdr lst) (+ n 1)))
          (else (loop (cdr lst) n)))))

; fold-left (reduce) - left-associative fold
(define (fold proc init lst)
  (%require-proper-list "fold" lst)
  (let loop ((lst lst) (acc init))
    (if (null? lst)
        acc
        (loop (cdr lst) (proc (car lst) acc)))))

; fold-right - right-associative fold
(define (fold-right proc init lst)
  (%require-proper-list "fold-right" lst)
  (if (null? lst)
      init
      (proc (car lst) (fold-right proc init (cdr lst)))))

; reduce - like fold but uses first element as initial value; returns
; ridentity for an empty list (SRFI-1 signature: (reduce f ridentity list))
(define (reduce proc ridentity lst)
  (if (null? lst)
      ridentity
      (fold proc (car lst) (cdr lst))))

; take - return first n elements of list (SRFI-1/R7RS-large signature:
; (take list k), matching MIT and every sibling function in this file)
(define (take lst n)
  (%require-nonnegative-integer "take" n)
  (let loop ((n n) (lst lst))
    (if (or (= n 0) (null? lst))
        '()
        (cons (car lst) (loop (- n 1) (cdr lst))))))

; drop - return list without first n elements (SRFI-1/R7RS-large
; signature: (drop list k))
(define (drop lst n)
  (%require-nonnegative-integer "drop" n)
  (let loop ((n n) (lst lst))
    (if (or (= n 0) (null? lst))
        lst
        (loop (- n 1) (cdr lst)))))

; partition - split list into two lists based on predicate
(define (partition pred lst)
  (%require-proper-list "partition" lst)
  (let loop ((lst lst) (yes '()) (no '()))
    (cond ((null? lst) (list (reverse yes) (reverse no)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no))
          (else (loop (cdr lst) yes (cons (car lst) no))))))

; zip - combine corresponding elements of two lists
(define (zip lst1 lst2)
  (%require-proper-list "zip" lst1)
  (%require-proper-list "zip" lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

; flatten - flatten nested list structure
(define (flatten lst)
  (if (pair? lst)
      (%require-proper-list "flatten" lst))
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

; last - return last element of list
(define (last lst)
  (car (last-pair lst)))

; iota - generate a list of count numbers: start, start+step, ...
; (SRFI-1/R7RS-large signature: (iota count [start [step]]))
(define (iota count . args)
  (let ((start (if (pair? args) (car args) 0))
        (step (if (and (pair? args) (pair? (cdr args))) (cadr args) 1)))
    (let loop ((i (- count 1)) (acc '()))
      (if (< i 0)
          acc
          (loop (- i 1) (cons (+ start (* i step)) acc))))))

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
  (%require-nonnegative-integer "make-list" n)
  (let ((f (if (null? fill) #f (car fill))))
    (let loop ((n n) (acc '()))
      (if (<= n 0)
          acc
          (loop (- n 1) (cons f acc))))))

; list-tabulate - create list by calling init-proc on indices 0..n-1
(define (list-tabulate n init-proc)
  (%require-nonnegative-integer "list-tabulate" n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0)
        acc
        (loop (- i 1) (cons (init-proc i) acc)))))

; list-copy - shallow copy of list; R7RS explicitly documents this on
; improper (dotted) lists too, preserving the non-null final cdr
(define (list-copy lst)
  (%require-finite-list "list-copy" lst)
  (if (pair? lst)
      (cons (car lst) (list-copy (cdr lst)))
      lst))

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
  (%require-proper-lists "list=" lists)
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

; Count the pairs in a (possibly improper) list, ignoring any final
; non-nil cdr - unlike `length`, which requires proper termination.
(define (%pair-count lst)
  (let loop ((lst lst) (n 0))
    (if (pair? lst) (loop (cdr lst) (+ n 1)) n)))

; take-right - return last n elements. SRFI-1/MIT permit a possibly
; improper (dotted) list, returning the final non-nil cdr along with the
; last n elements if n reaches all the way to it.
(define (take-right lst n)
  (%require-finite-list "take-right" lst)
  (%require-nonnegative-integer "take-right" n)
  (let ((len (%pair-count lst)))
    (if (>= n len)
        lst
        (drop lst (- len n)))))

; drop-right - return all but last n elements
(define (drop-right lst n)
  (%require-finite-list "drop-right" lst)
  (%require-nonnegative-integer "drop-right" n)
  (let ((len (%pair-count lst)))
    (if (>= n len)
        '()
        (take lst (- len n)))))

; split-at - split list at index, return two lists
(define (split-at lst n)
  (%require-nonnegative-integer "split-at" n)
  (values (take lst n) (drop lst n)))

;;; ============================================================================
;;; Additional SRFI-1 Miscellaneous
;;; ============================================================================

; concatenate - append all lists in list-of-lists
(define (concatenate lists)
  (%require-proper-lists "concatenate" lists)
  (apply append lists))

; append-reverse - (append (reverse rev-head) tail)
(define (append-reverse rev-head tail)
  (%require-proper-list "append-reverse" rev-head)
  (let loop ((rev-head rev-head) (tail tail))
    (if (null? rev-head)
        tail
        (loop (cdr rev-head) (cons (car rev-head) tail)))))

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
  (%require-proper-list "pair-fold" lst)
  (let loop ((lst lst) (acc init))
    (if (null? lst)
        acc
        (loop (cdr lst) (proc lst acc)))))

; pair-fold-right - like fold-right but proc receives pairs
(define (pair-fold-right proc init lst)
  (%require-proper-list "pair-fold-right" lst)
  (if (null? lst)
      init
      (proc lst (pair-fold-right proc init (cdr lst)))))

; reduce-right - like reduce but right-associative
(define (reduce-right proc init lst)
  (%require-proper-list "reduce-right" lst)
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
  (%require-proper-list "filter-map" lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        (reverse acc)
        (let ((result (proc (car lst))))
          (loop (cdr lst) (if result (cons result acc) acc))))))

; pair-for-each - like for-each but proc receives pairs
(define (pair-for-each proc lst)
  (%require-proper-list "pair-for-each" lst)
  (if (not (null? lst))
      (begin
        (proc lst)
        (pair-for-each proc (cdr lst)))))

;;; ============================================================================
;;; Additional SRFI-1 Searching
;;; ============================================================================

; find-tail - return tail of list starting at first match
(define (find-tail pred lst)
  (%require-proper-list "find-tail" lst)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((pred (car lst)) lst)
          (else (loop (cdr lst))))))

; list-index - return index of first element satisfying pred
(define (list-index pred lst)
  (%require-proper-list "list-index" lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
          ((pred (car lst)) i)
          (else (loop (cdr lst) (+ i 1))))))

; take-while - return longest prefix satisfying pred
(define (take-while pred lst)
  (%require-proper-list "take-while" lst)
  (let loop ((lst lst))
    (if (or (null? lst) (not (pred (car lst))))
        '()
        (cons (car lst) (loop (cdr lst))))))

; drop-while - drop longest prefix satisfying pred
(define (drop-while pred lst)
  (%require-proper-list "drop-while" lst)
  (let loop ((lst lst))
    (cond ((null? lst) '())
          ((pred (car lst)) (loop (cdr lst)))
          (else lst))))

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
  (%require-proper-list "delete-duplicates" lst)
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
  (%require-proper-list "alist-copy" alist)
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
  (%require-proper-list "take!" lst)
  (%require-nonnegative-integer "take!" n)
  (cond ((<= n 0) '())
        ((null? lst) '())
        (else
         (let ((tail (drop lst (- n 1))))
           (if (null? tail)
               lst
               (begin
                 (set-cdr! tail '())
                 lst))))))

; drop-right! - destructive version of drop-right
(define (drop-right! lst n)
  (%require-proper-list "drop-right!" lst)
  (%require-nonnegative-integer "drop-right!" n)
  (let ((len (length lst)))
    (if (<= (- len n) 0)
        '()
        (take! lst (- len n)))))

; split-at! - destructive version of split-at
(define (split-at! lst n)
  (%require-proper-list "split-at!" lst)
  (%require-nonnegative-integer "split-at!" n)
  (if (<= n 0)
      (values '() lst)
      (let ((tail (drop lst (- n 1))))
        (if (null? tail)
            (values lst '())
            (let ((rest (cdr tail)))
              (set-cdr! tail '())
              (values lst rest))))))

; append! - destructive append. Every argument but the last must be a
; proper list; the last may be any object (including improper), matching
; R7RS `append` and this file's own C-primitive `append`.
(define (append! . lists)
  (%require-proper-lists-but-last "append!" lists)
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
  (%require-proper-lists "concatenate!" list-of-lists)
  (apply append! list-of-lists))

; reverse! - destructive reverse
(define (reverse! lst)
  (%require-proper-list "reverse!" lst)
  (let loop ((prev '()) (curr lst))
    (if (null? curr)
        prev
        (let ((next (cdr curr)))
          (set-cdr! curr prev)
          (loop curr next)))))

; append-reverse! - destructive append-reverse
(define (append-reverse! rev-head tail)
  (%require-proper-list "append-reverse!" rev-head)
  (let loop ((curr rev-head) (acc tail))
    (if (null? curr)
        acc
        (let ((next (cdr curr)))
          (set-cdr! curr acc)
          (loop next curr)))))

; map! - destructive map (mutates first list)
(define (map! proc lst . lsts)
  (%require-proper-list "map!" lst)
  (%require-proper-lists "map!" lsts)
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
  (%require-proper-list "filter!" lst)
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
  (%require-proper-list "take-while!" lst)
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
  (%require-proper-list "span!" lst)
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
  (%require-proper-list "delete-duplicates!" lst)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (let loop ((curr lst))
      (if (null? curr)
          lst
          (begin
            (set-cdr! curr (delete! (car curr) (cdr curr) eq))
            (loop (cdr curr)))))))

; alist-delete! - destructive alist-delete
(define (alist-delete! key alist . maybe-eq)
  (%require-proper-list "alist-delete!" alist)
  (let ((eq (if (null? maybe-eq) equal? (car maybe-eq))))
    (filter! (lambda (pair) (not (eq key (car pair)))) alist)))

;;; ============================================================================
;;; Dynamic-wind (R5RS)
;;; ============================================================================

; Wind stack: list of (before . after) pairs
(define *wind-stack* '())

; Helper: execute wind thunks when jumping between continuation points.
; `from`/`to` are wind-stacks (innermost frame at the head, see
; *wind-stack* above), and any real common ancestor context corresponds to
; a common TAIL of the two lists - so this is the classic "find the
; convergence point of two lists that may differ in length" problem.
; Popping one frame off *each* side per step (as a naive implementation
; might) only finds that point when the lists happen to have equal
; length; when they don't, the shorter side hits '() first and the
; unequal-length remainder of the longer side gets misidentified as
; divergent, spuriously unwinding/rewinding dynamic-wind frames that were
; never actually left. Fix: first equalize lengths by consuming the
; longer list's extra head frames (running their thunks - after-thunks
; for `from`, before-thunks for `to`), then walk the equal-length
; remainders together to find the true common tail.
(define (do-wind from to)
  (define (length-of lst)
    (let loop ((l lst) (n 0)) (if (null? l) n (loop (cdr l) (+ n 1)))))
  ; Run after-thunks for the n innermost (head) frames of `from` that have
  ; no counterpart in `to` because `to` is shorter. Innermost-first order
  ; (head to tail) is correct for unwinding.
  (define (unwind-extra from n)
    (if (= n 0)
        from
        (begin
          (set! *wind-stack* from)
          ((cdar from))
          (unwind-extra (cdr from) (- n 1)))))
  ; Run before-thunks for the n innermost (head) frames of `to` that have
  ; no counterpart in `from` because `from` is shorter. Must run
  ; outermost-first (reverse of `to`'s head-to-tail order), so recurse
  ; down to the equal-length point first and run each thunk as the
  ; recursion returns.
  (define (rewind-extra to n)
    (if (= n 0)
        to
        (let ((rest (rewind-extra (cdr to) (- n 1))))
          (set! *wind-stack* to)
          ((caar to))
          rest)))
  ; `from`/`to` now have equal length. Find the common tail: unwind from's
  ; divergent prefix innermost-first (before recursing), then rewind to's
  ; divergent prefix outermost-first (after recursing).
  (define (converge from to)
    (if (eq? from to)
        to
        (begin
          (set! *wind-stack* from)
          ((cdar from))
          (let ((common (converge (cdr from) (cdr to))))
            (set! *wind-stack* to)
            ((caar to))
            common))))
  (unless (eq? from to)
    (let* ((from-len (length-of from))
           (to-len (length-of to))
           (from1 (if (> from-len to-len)
                      (unwind-extra from (- from-len to-len))
                      from))
           (to1 (if (> to-len from-len)
                    (rewind-extra to (- to-len from-len))
                    to)))
      (converge from1 to1)))
  (set! *wind-stack* to))

; dynamic-wind: establish before/after thunks around body
(define dynamic-wind
  (lambda (before body after)
    (before)
    (set! *wind-stack* (cons (cons before after) *wind-stack*))
    (let ((result (body)))
      (set! *wind-stack* (cdr *wind-stack*))
      (after)
      result)))

; Wrap call/cc to handle dynamic-wind
(define call-with-current-continuation
  (let ((primitive-call/cc call-with-current-continuation))
    (lambda (proc)
      (let ((winds *wind-stack*))
        (primitive-call/cc
         (lambda (cont)
           (proc (lambda vals
                   (do-wind *wind-stack* winds)
                   (apply cont vals)))))))))

(define call/cc call-with-current-continuation)

;;; ============================================================================
;;; SRFI-39/R7RS parameters
;;; ============================================================================

;; Private sentinel: a second argument eq? to this tells the parameter
;; procedure to store its first argument as-is, bypassing the converter.
;; parameterize uses this to restore the old value on exit - that value is
;; already-converted (it came from a previous call to the parameter), so
;; running it through the converter again would apply it twice and corrupt
;; state for any converter that isn't idempotent (R7RS 4.2.6: the dynamic
;; extent's original value is restored, not reconverted).
(define %parameter-raw-set-tag (list 'parameter-raw-set))

(define (make-parameter init . maybe-converter)
  (let ((converter (if (null? maybe-converter)
                       (lambda (x) x)
                       (car maybe-converter)))
        (cell (vector 'parameter #f)))
    (vector-set! cell 1 (converter init))
    (lambda args
      (cond ((null? args)
             (vector-ref cell 1))
            ((null? (cdr args))
             (vector-set! cell 1 (converter (car args))))
            ((and (null? (cddr args)) (eq? (cadr args) %parameter-raw-set-tag))
             (vector-set! cell 1 (car args)))
            (else
             (error "parameter: expected zero or one argument"))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param value) rest ...) body ...)
     (let ((p param)
           (new-value value))
       (let ((old-value (p)))
         (dynamic-wind
          (lambda () (p new-value))
          (lambda () (parameterize (rest ...) body ...))
          (lambda () (p old-value %parameter-raw-set-tag))))))))

;;; ============================================================================
;;; SRFI-9: Defining Record Types
;;; ============================================================================

;; define-record-type creates a new record type with:
;; - A constructor procedure
;; - A predicate to test for instances
;; - Accessor and mutator procedures for each field
;;
;; Syntax:
;; (define-record-type <type-name>
;;   (<constructor-name> <field-name> ...)
;;   <predicate-name>
;;   (<field-name> <accessor-name>)
;;   (<field-name> <accessor-name> <mutator-name>)
;;   ...)
;;
;; Implementation uses vectors: #(<type-tag> <field1> <field2> ...)

(define (record-constructor-field-ref field bindings)
  (cond ((assq field bindings) => cdr)
        (else #f)))

(define (record-any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (record-any pred (cdr lst)))))

(define (record-duplicates? lst)
  (cond ((null? lst) #f)
        ((memq (car lst) (cdr lst)) #t)
        (else (record-duplicates? (cdr lst)))))

(define (record-field-spec-name spec)
  (if (and (pair? spec) (symbol? (car spec)))
      (car spec)
      (error "define-record-type: invalid field spec" spec)))

(define (record-field-spec-accessor spec)
  (if (and (pair? spec) (pair? (cdr spec)) (symbol? (cadr spec))
           (or (null? (cddr spec))
               (and (pair? (cddr spec)) (null? (cdddr spec))
                    (symbol? (caddr spec)))))
      (cadr spec)
      (error "define-record-type: invalid field spec" spec)))

(define (record-field-spec-mutator spec)
  (if (and (pair? spec) (pair? (cdr spec)) (pair? (cddr spec))
           (null? (cdddr spec)) (symbol? (caddr spec)))
      (caddr spec)
      #f))

(define (record-remove-false lst)
  (cond ((null? lst) '())
        ((car lst) (cons (car lst) (record-remove-false (cdr lst))))
        (else (record-remove-false (cdr lst)))))

(define (validate-record-type-spec type-name constructor-name constructor-fields
                                   predicate-name field-specs)
  (if (not (symbol? type-name))
      (error "define-record-type: type name must be a symbol" type-name))
  (if (not (symbol? constructor-name))
      (error "define-record-type: constructor name must be a symbol" constructor-name))
  (if (not (symbol? predicate-name))
      (error "define-record-type: predicate name must be a symbol" predicate-name))
  (if (record-any (lambda (field) (not (symbol? field))) constructor-fields)
      (error "define-record-type: constructor fields must be symbols"
             constructor-fields))
  (if (record-duplicates? constructor-fields)
      (error "define-record-type: duplicate constructor field"
             constructor-fields))
  (let ((field-names (map record-field-spec-name field-specs)))
    (if (record-duplicates? field-names)
        (error "define-record-type: duplicate field name" field-names))
    (for-each
     (lambda (field)
       (if (not (memq field field-names))
           (error "define-record-type: constructor field is not a record field"
                  field)))
     constructor-fields)
    (let* ((accessors (map record-field-spec-accessor field-specs))
           (mutators (record-remove-false
                      (map record-field-spec-mutator field-specs)))
           (bindings (append (list type-name constructor-name predicate-name)
                             accessors
                             mutators)))
      (if (record-duplicates? bindings)
          (error "define-record-type: duplicate generated binding" bindings))))
  #t)

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type-name
       (constructor-name constructor-field ...)
       predicate-name
       field-spec ...)
     (begin
       (validate-record-type-spec 'type-name
                                  'constructor-name
                                  '(constructor-field ...)
                                  'predicate-name
                                  '(field-spec ...))
       ;; The constructor closure itself is a unique, stable type tag.  Avoid
       ;; an introduced top-level helper binding here: repeated macro uses
       ;; would otherwise share that binding and overwrite an earlier tag.
       (define-record-constructor constructor-name
         (constructor-field ...)
         field-spec ...)

       ;; Predicate: checks if value is a vector with matching type tag
       (define predicate-name
         (let ((record-type-tag constructor-name))
           (lambda (obj)
             (and (vector? obj)
                  (> (vector-length obj) 0)
                  (eq? (vector-ref obj 0) record-type-tag)))))

       ;; Generate field accessors and mutators
       (define-record-fields type-name predicate-name 1 field-spec ...)))))

(define-syntax define-record-constructor
  (syntax-rules ()
    ((define-record-constructor constructor-name
       (constructor-field ...)
       field-spec ...)
     (define (constructor-name constructor-field ...)
       (list->vector
         (cons constructor-name
               (define-record-constructor-values
                 (list (cons 'constructor-field constructor-field) ...)
                 field-spec ...)))))))

(define-syntax define-record-constructor-values
  (syntax-rules ()
    ((define-record-constructor-values bindings)
     '())
    ((define-record-constructor-values bindings
       (field-name accessor-name)
       rest ...)
     (cons (record-constructor-field-ref 'field-name bindings)
           (define-record-constructor-values bindings rest ...)))
    ((define-record-constructor-values bindings
       (field-name accessor-name mutator-name)
       rest ...)
     (cons (record-constructor-field-ref 'field-name bindings)
           (define-record-constructor-values bindings rest ...)))))

;; Helper macro to define field accessors/mutators
;; Index starts at 1 because position 0 is the type tag
(define-syntax define-record-fields
  (syntax-rules ()
    ;; Base case: no more fields
    ((define-record-fields type-name predicate-name index)
     (begin))

    ;; Field with accessor only
    ((define-record-fields type-name predicate-name index
       (field-name accessor-name)
       rest ...)
     (begin
       (define (accessor-name obj)
         (if (predicate-name obj)
             (vector-ref obj index)
             (error "record accessor: wrong record type" obj)))
       (define-record-fields type-name predicate-name (+ index 1) rest ...)))

    ;; Field with accessor and mutator
    ((define-record-fields type-name predicate-name index
       (field-name accessor-name mutator-name)
       rest ...)
     (begin
       (define (accessor-name obj)
         (if (predicate-name obj)
             (vector-ref obj index)
             (error "record accessor: wrong record type" obj)))
       (define (mutator-name obj val)
         (if (predicate-name obj)
             (vector-set! obj index val)
             (error "record mutator: wrong record type" obj)))
       (define-record-fields type-name predicate-name (+ index 1) rest ...)))))

;;; ============================================================================
;;; SRFI-26: Notation for Specializing Parameters (cut/cute)
;;; ============================================================================

;; cut creates a procedure by specializing some parameters of another procedure.
;; <> is a slot marker for parameters to be filled in later.
;; <...> is a rest-slot marker for remaining arguments.
;;
;; Examples:
;; (cut cons (+ a 1) <>)      => (lambda (x) (cons (+ a 1) x))
;; (cut list 1 <> 3 <> 5)     => (lambda (x y) (list 1 x 3 y 5))
;; (cut list 1 <> 3 <...>)    => (lambda (x . rest) (apply list 1 x 3 rest))
;; (cut list)                 => (lambda () (list))

(define-syntax cut
  (syntax-rules (<> <...>)
    ;; Entry point: start collecting
    ((cut slot-or-expr ...)
     (cut-helper () () (slot-or-expr ...)))))

(define-syntax cut-helper
  (syntax-rules (<> <...>)
    ;; Base case: no more slots/exprs, no rest-slot
    ((cut-helper (param ...) (arg ...) ())
     (lambda (param ...) (arg ...)))

    ;; Rest-slot at the end
    ((cut-helper (param ...) (arg ...) (<...>))
     (lambda (param ... . rest-args) (apply arg ... rest-args)))

    ;; Slot: add a fresh parameter. Explicit states avoid reusing one
    ;; introduced identifier for every <>.
    ((cut-helper () (arg ...) (<> . rest))
     (cut-helper (x1) (arg ... x1) rest))
    ((cut-helper (x1) (arg ...) (<> . rest))
     (cut-helper (x1 x2) (arg ... x2) rest))
    ((cut-helper (x1 x2) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3) (arg ... x3) rest))
    ((cut-helper (x1 x2 x3) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3 x4) (arg ... x4) rest))
    ((cut-helper (x1 x2 x3 x4) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3 x4 x5) (arg ... x5) rest))
    ((cut-helper (x1 x2 x3 x4 x5) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3 x4 x5 x6) (arg ... x6) rest))
    ((cut-helper (x1 x2 x3 x4 x5 x6) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3 x4 x5 x6 x7) (arg ... x7) rest))
    ((cut-helper (x1 x2 x3 x4 x5 x6 x7) (arg ...) (<> . rest))
     (cut-helper (x1 x2 x3 x4 x5 x6 x7 x8) (arg ... x8) rest))
    ((cut-helper (x1 x2 x3 x4 x5 x6 x7 x8) (arg ...) (<> . rest))
     (error "cut: too many slots"))

    ;; Expression: evaluate and add to args
    ((cut-helper (param ...) (arg ...) (expr . rest))
     (cut-helper (param ...) (arg ... expr) rest))))

;; cute is like cut but evaluates non-slot expressions at definition time
;; (not at call time like cut does).
;;
;; Example:
;; (cute cons (+ a 1) <>) evaluates (+ a 1) once when cute form is evaluated,
;; while (cut cons (+ a 1) <>) evaluates it on each call.

(define-syntax cute
  (syntax-rules (<> <...>)
    ;; Entry point: start collecting
    ((cute slot-or-expr ...)
     (cute-helper () () (slot-or-expr ...)))))

(define-syntax cute-helper
  (syntax-rules (<> <...>)
    ;; Base case: no more slots/exprs, no rest-slot
    ((cute-helper (param ...) (arg ...) ())
     (lambda (param ...) (arg ...)))

    ;; Rest-slot at the end
    ((cute-helper (param ...) (arg ...) (<...>))
     (lambda (param ... . rest-args) (apply arg ... rest-args)))

    ;; Slot: add a fresh parameter. Explicit states avoid reusing one
    ;; introduced identifier for every <>.
    ((cute-helper () (arg ...) (<> . rest))
     (cute-helper (x1) (arg ... x1) rest))
    ((cute-helper (x1) (arg ...) (<> . rest))
     (cute-helper (x1 x2) (arg ... x2) rest))
    ((cute-helper (x1 x2) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3) (arg ... x3) rest))
    ((cute-helper (x1 x2 x3) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3 x4) (arg ... x4) rest))
    ((cute-helper (x1 x2 x3 x4) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3 x4 x5) (arg ... x5) rest))
    ((cute-helper (x1 x2 x3 x4 x5) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3 x4 x5 x6) (arg ... x6) rest))
    ((cute-helper (x1 x2 x3 x4 x5 x6) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3 x4 x5 x6 x7) (arg ... x7) rest))
    ((cute-helper (x1 x2 x3 x4 x5 x6 x7) (arg ...) (<> . rest))
     (cute-helper (x1 x2 x3 x4 x5 x6 x7 x8) (arg ... x8) rest))
    ((cute-helper (x1 x2 x3 x4 x5 x6 x7 x8) (arg ...) (<> . rest))
     (error "cute: too many slots"))

    ;; Expression: wrap in a let to evaluate once, using nested approach
    ((cute-helper (param ...) (arg ...) (expr . rest))
     (let ((tmp expr))
       (cute-helper (param ...) (arg ... tmp) rest)))))

;;; ============================================================================
;;; Sort Utilities
;;; ============================================================================

(define (merge less? left right)
  (%require-proper-list "merge" left)
  (%require-proper-list "merge" right)
  (cond ((null? left) right)
        ((null? right) left)
        ((less? (car right) (car left))
         (cons (car right) (merge less? left (cdr right))))
        (else
         (cons (car left) (merge less? (cdr left) right)))))

(define (list-sort less? lst)
  (%require-proper-list "list-sort" lst)
  (let split ((xs lst) (slow lst) (fast lst) (front '()))
    (if (or (null? fast) (null? (cdr fast)))
        (let ((left (reverse front))
              (right slow))
          (cond ((null? right) left)
                ((null? (cdr right)) (merge less? left right))
                (else (merge less?
                             (list-sort less? left)
                             (list-sort less? right)))))
        (split xs (cdr slow) (cddr fast) (cons (car slow) front)))))

(define (stable-sort sequence less?)
  (if (vector? sequence)
      (list->vector (list-sort less? (vector->list sequence)))
      (list-sort less? sequence)))

(define sort stable-sort)

(define (vector-sort less? vec)
  (list->vector (list-sort less? (vector->list vec))))

(define (vector-sort! less? vec)
  (let ((sorted (vector-sort less? vec)))
    (let loop ((i 0))
      (if (< i (vector-length vec))
          (begin
            (vector-set! vec i (vector-ref sorted i))
            (loop (+ i 1)))
          vec))))

(define (sort! sequence less?)
  (if (vector? sequence)
      (vector-sort! less? sequence)
      (list-sort less? sequence)))

;;; ============================================================================
;;; Format (SRFI-28 basic format strings)
;;; ============================================================================

;; format - formatted output
;; Supports: ~a, ~s, ~d, ~x, ~o, ~b, ~r, ~c, ~%, ~&, ~_
;; First argument can be:
;;   #f - return string
;;   #t - output to current-output-port, return unspecified
;;   port - output to port, return unspecified
(define (format dest fmt . args)
  (define (require-arg args directive)
    (if (null? args)
        (error (string-append "format: not enough arguments for ~" directive))
        (car args)))

  (define (format-radix args radix directive port)
    (let ((arg (require-arg args directive)))
      (display (number->string arg radix) port)
      (cdr args)))

  (define (format-to-string)
    (let ((out (open-output-string)))
      (format-to-port out)
      (get-output-string out)))

  ; Scan an optional decimal radix parameter between ~ and the directive
  ; letter (e.g. ~16r), returning (values radix-or-#f index-after-digits).
  (define (scan-radix-param i)
    (let loop ((j i) (n #f))
      (if (and (< j (string-length fmt)) (char-numeric? (string-ref fmt j)))
          (loop (+ j 1)
                (+ (* (or n 0) 10) (digit-value (string-ref fmt j))))
          (values n j))))

  (define (format-to-port port)
    (let loop ((i 0) (args args))
      (if (< i (string-length fmt))
          (let ((c (string-ref fmt i)))
            (if (char=? c #\~)
                (if (< (+ i 1) (string-length fmt))
                    (call-with-values
                      (lambda () (scan-radix-param (+ i 1)))
                      (lambda (radix-param j)
                        (if (>= j (string-length fmt))
                            (error "format: incomplete escape at end of string")
                            (let ((directive (string-ref fmt j))
                                  (next (+ j 1)))
                      (cond
                        ((or (char=? directive #\a) (char=? directive #\A))
                         (display (require-arg args "a") port)
                         (loop next (cdr args)))
                        ((or (char=? directive #\s) (char=? directive #\S))
                         (write (require-arg args "s") port)
                         (loop next (cdr args)))
                        ((or (char=? directive #\d) (char=? directive #\D))
                         (loop next (format-radix args 10 "d" port)))
                        ((or (char=? directive #\x) (char=? directive #\X))
                         (loop next (format-radix args 16 "x" port)))
                        ((or (char=? directive #\o) (char=? directive #\O))
                         (loop next (format-radix args 8 "o" port)))
                        ((or (char=? directive #\b) (char=? directive #\B))
                         (loop next (format-radix args 2 "b" port)))
                        ((or (char=? directive #\r) (char=? directive #\R))
                         ; ~NNr prints in radix NN (e.g. ~16r for hex);
                         ; bare ~r defaults to radix 10, like ~d
                         (loop next
                               (format-radix args (or radix-param 10) "r"
                                            port)))
                        ((or (char=? directive #\c) (char=? directive #\C))
                         (let ((arg (require-arg args "c")))
                           (if (char? arg)
                               (write-char arg port)
                               (error "format: ~c expects a character"))
                           (loop next (cdr args))))
                        ((char=? directive #\%)
                         (newline port)
                         (loop next args))
                        ((or (char=? directive #\&) (char=? directive #\_))
                         (newline port)
                         (loop next args))
                        ((char=? directive #\~)
                         (write-char #\~ port)
                         (loop next args))
                        (else
                         (error "format: unknown directive" directive)))))))
                    (error "format: incomplete escape at end of string"))
                (begin
                  (write-char c port)
                  (loop (+ i 1) args))))
          #t)))

  (cond
    ((eq? dest #f) (format-to-string))
    ((eq? dest #t) (format-to-port (current-output-port)))
    ((output-port? dest) (format-to-port dest))
    (else (error "format: invalid destination" dest))))

;;; ============================================================================
;;; String Utilities
;;; ============================================================================

(define (unicode-char . codes)
  (list->string (map integer->char codes)))

(define (string-normalized-nfd? s)
  (string=? s (string-normalize-nfd s)))

(define (string-normalized-nfc? s)
  (string=? s (string-normalize-nfc s)))

(define (string-normalized-nfkd? s)
  (string=? s (string-normalize-nfkd s)))

(define (string-normalized-nfkc? s)
  (string=? s (string-normalize-nfkc s)))

(define (string-ci=? a b)
  (string=? (string-foldcase a) (string-foldcase b)))

(define (string-ci<? a b)
  (string<? (string-foldcase a) (string-foldcase b)))

(define (string-ci>? a b)
  (string>? (string-foldcase a) (string-foldcase b)))

(define (string-ci<=? a b)
  (string<=? (string-foldcase a) (string-foldcase b)))

(define (string-ci>=? a b)
  (string>=? (string-foldcase a) (string-foldcase b)))

;; string-prefix? - check if s starts with prefix
(define (string-prefix? prefix s)
  (let ((plen (string-length prefix))
        (slen (string-length s)))
    (and (>= slen plen)
         (string=? prefix (substring s 0 plen)))))

;; string-suffix? - check if s ends with suffix
(define (string-suffix? suffix s)
  (let ((xlen (string-length suffix))
        (slen (string-length s)))
    (and (>= slen xlen)
         (string=? suffix (substring s (- slen xlen) slen)))))

;; string-trim - remove leading/trailing whitespace
(define (string-trim s)
  (string-trim-right (string-trim-left s)))

;; string-trim-left - remove leading whitespace
(define (string-trim-left s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (>= i len)
          ""
          (if (char-whitespace? (string-ref s i))
              (loop (+ i 1))
              (substring s i len))))))

;; string-trim-right - remove trailing whitespace
(define (string-trim-right s)
  (let ((len (string-length s)))
    (let loop ((i (- len 1)))
      (if (< i 0)
          ""
          (if (char-whitespace? (string-ref s i))
              (loop (- i 1))
              (substring s 0 (+ i 1)))))))

;; string-split - split string by delimiter (default: whitespace)
(define (string-split s . args)
  (let ((delim (if (null? args) #f (car args))))
    (if delim
        ;; Split by specific character
        (let loop ((i 0) (start 0) (result '()))
          (if (>= i (string-length s))
              (reverse (if (> i start)
                           (cons (substring s start i) result)
                           result))
              (if (char=? (string-ref s i) delim)
                  (loop (+ i 1) (+ i 1)
                        (if (> i start)
                            (cons (substring s start i) result)
                            result))
                  (loop (+ i 1) start result))))
        ;; Split by whitespace
        (let loop ((i 0) (start #f) (result '()))
          (if (>= i (string-length s))
              (reverse (if start
                           (cons (substring s start i) result)
                           result))
              (let ((c (string-ref s i)))
                (if (char-whitespace? c)
                    (loop (+ i 1) #f
                          (if start
                              (cons (substring s start i) result)
                              result))
                    (loop (+ i 1) (or start i) result))))))))

;;; ============================================================================
;;; SRFI-8: receive (binding to multiple values)
;;; ============================================================================

;; (receive (var ...) producer body ...)
;; Binds vars to values returned by producer, then evaluates body
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
       (lambda formals body ...)))))

;;; ============================================================================
;;; R7RS multiple-value binding forms
;;; ============================================================================

;; A multi-value <formals> is the same grammar as a lambda's parameter list:
;; (var ...), a bare identifier (collects all values as a list), or a
;; dotted (var ... . rest). %define-values-declare!/define-values-set!
;; recurse on it the same way a variadic lambda would - matching (var . more)
;; first (a pair, whether the overall shape is proper or dotted) and falling
;; back to a bare identifier only once that no longer matches.
(define-syntax %define-values-declare!
  (syntax-rules ()
    ((%define-values-declare! ()) (if #f #f))
    ((%define-values-declare! (var . more))
     (begin (define var #f) (%define-values-declare! more)))
    ((%define-values-declare! var) (define var #f))))

(define-syntax define-values-set!
  (syntax-rules ()
    ((define-values-set! () vals)
     (if #f #f))
    ((define-values-set! (var . more) vals)
     (begin
       (set! var (car vals))
       (define-values-set! more (cdr vals))))
    ((define-values-set! var vals)
     (set! var vals))))

(define-syntax define-values
  (syntax-rules ()
    ((define-values formals expression)
     (begin
       (%define-values-declare! formals)
       (let ((vals (call-with-values (lambda () expression) list)))
         (define-values-set! formals vals))))))

(define-syntax let-values-bind
  (syntax-rules ()
    ((let-values-bind () body ...)
     (let () body ...))
    ((let-values-bind ((vals formals) rest ...) body ...)
     (call-with-values
       (lambda () (apply values vals))
       (lambda formals
         (let-values-bind (rest ...) body ...))))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((formals expression) ...) body ...)
     (let ((vals (call-with-values (lambda () expression) list)) ...)
       (let-values-bind ((vals formals) ...) body ...)))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (let () body ...))
    ((let*-values ((formals expression) rest ...) body ...)
     (call-with-values
       (lambda () expression)
       (lambda formals
         (let*-values (rest ...) body ...))))))

;;; ============================================================================
;;; R7RS case-lambda
;;; ============================================================================

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda clause ...)
     (lambda args
       (case-lambda-dispatch
         args
         (list (case-lambda-clause clause) ...))))))

(define-syntax case-lambda-clause
  (syntax-rules ()
    ((case-lambda-clause (formals body ...))
     (cons 'formals (lambda formals body ...)))))

(define (case-lambda-formals-match? formals argc)
  (let loop ((xs formals) (n argc))
    (cond
      ((symbol? xs) #t)
      ((null? xs) (= n 0))
      ((pair? xs) (and (> n 0) (loop (cdr xs) (- n 1))))
      (else #f))))

(define (case-lambda-dispatch args clauses)
  (let ((argc (length args)))
    (let loop ((xs clauses))
      (if (null? xs)
          (error "case-lambda: no matching clause")
          (let ((clause (car xs)))
            (if (case-lambda-formals-match? (car clause) argc)
                (apply (cdr clause) args)
                (loop (cdr xs))))))))

;;; ============================================================================
;;; Hash-table utilities
;;; ============================================================================

(define (hash-table-ref/default table key default)
  (hash-table-ref table key default))

(define (hash-table-update! table key proc . maybe-get-default)
  ; The optional 4th argument is a THUNK invoked to produce the default
  ; when key is absent (SRFI-69/R7RS-large convention, matches MIT), not
  ; a literal default value.
  (let ((current
          (if (hash-table-exists? table key)
              (hash-table-ref table key)
              (if (null? maybe-get-default)
                  (error "hash-table-update!: key not found" key)
                  ((car maybe-get-default))))))
    (hash-table-set! table key (proc current))))

(define (hash-table-walk table proc)
  (for-each (lambda (entry)
              (proc (car entry) (cdr entry)))
            (hash-table->alist table)))

(define (hash-table-for-each proc table)
  (hash-table-walk table proc))

(define (hash-table-fold proc init table)
  (let loop ((entries (hash-table->alist table)) (acc init))
    (if (null? entries)
        acc
        (let ((entry (car entries)))
          (loop (cdr entries)
                (proc (car entry) (cdr entry) acc))))))

(define (hash-table-map proc table)
  (map (lambda (entry) (proc (car entry) (cdr entry)))
       (hash-table->alist table)))

(define (hash-table-copy table . maybe-mutable)
  (let ((copy (make-hash-table (hash-table-size table))))
    (hash-table-walk table
      (lambda (key value)
        (hash-table-set! copy key value)))
    copy))

;;; ============================================================================
;;; Vector and string library additions
;;; ============================================================================

(define (vector-copy vec . rest)
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (vector-length vec)))
         (len (- end start))
         (out (make-vector len)))
    (let loop ((i 0))
      (if (= i len)
          out
          (begin
            (vector-set! out i (vector-ref vec (+ start i)))
            (loop (+ i 1)))))))

(define (vector-copy! to at from . rest)
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (vector-length from)))
         (tmp (vector-copy from start end))
         (len (vector-length tmp)))
    (let loop ((i 0))
      (if (= i len)
          (if #f #f)
          (begin
            (vector-set! to (+ at i) (vector-ref tmp i))
            (loop (+ i 1)))))))

(define (vector-append . vecs)
  (list->vector (apply append (map vector->list vecs))))

(define (vector-map proc vec . vecs)
  (list->vector (apply map proc (vector->list vec) (map vector->list vecs))))

(define (vector-for-each proc vec . vecs)
  (apply for-each proc (vector->list vec) (map vector->list vecs)))

(define (string-map proc str . strs)
  (list->string (apply map proc (string->list str) (map string->list strs))))

(define (string-for-each proc str . strs)
  (apply for-each proc (string->list str) (map string->list strs)))

(define (string* objects)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (obj) (display obj port)) objects))))

(define (string-append* strings)
  (apply string-append strings))

(define (string-compare string1 string2 if-eq if-lt if-gt)
  (cond ((string=? string1 string2) (if-eq))
        ((string<? string1 string2) (if-lt))
        (else (if-gt))))

(define (string-compare-ci string1 string2 if-eq if-lt if-gt)
  (string-compare (string-foldcase string1)
                  (string-foldcase string2)
                  if-eq
                  if-lt
                  if-gt))

(define (string-upper-case? str)
  (let loop ((chars (string->list str)) (saw-letter? #f))
    (cond ((null? chars) saw-letter?)
          ((char-alphabetic? (car chars))
           (and (char-upper-case? (car chars))
                (loop (cdr chars) #t)))
          (else
           (loop (cdr chars) saw-letter?)))))

(define (string-lower-case? str)
  (let loop ((chars (string->list str)) (saw-letter? #f))
    (cond ((null? chars) saw-letter?)
          ((char-alphabetic? (car chars))
           (and (char-lower-case? (car chars))
                (loop (cdr chars) #t)))
          (else
           (loop (cdr chars) saw-letter?)))))

(define (string-count proc str . strs)
  (let ((count 0))
    (apply string-for-each
           (lambda chars
             (if (apply proc chars)
                 (set! count (+ count 1))))
           str
           strs)
    count))

(define (string-any proc str . strs)
  (let loop ((char-lists (cons (string->list str) (map string->list strs))))
    (if (or (null? char-lists) (any null? char-lists))
        #f
        (let ((value (apply proc (map car char-lists))))
          (if value
              value
              (loop (map cdr char-lists)))))))

(define (string-every proc str . strs)
  (let loop ((char-lists (cons (string->list str) (map string->list strs)))
             (last #t))
    (if (or (null? char-lists) (any null? char-lists))
        last
        (let ((value (apply proc (map car char-lists))))
          (if value
              (loop (map cdr char-lists) value)
              #f)))))

(define (string-null? str)
  (= (string-length str) 0))

(define (string-head str end)
  (substring str 0 end))

(define (string-tail str start)
  (substring str start))

(define (string-hash str . maybe-modulus)
  (let ((modulus (if (null? maybe-modulus) #f (car maybe-modulus))))
    (let loop ((chars (string->list str)) (hash 5381))
      (if (null? chars)
          (if modulus (modulo hash modulus) hash)
          (loop (cdr chars)
                (+ (* hash 33) (char->integer (car chars))))))))

(define (string-hash-ci str . maybe-modulus)
  (apply string-hash (string-foldcase str) maybe-modulus))

(define (string-builder . maybe-buffer-length)
  (let ((chunks '())
        (count 0))
    (lambda args
      (cond
        ((null? args)
         (apply string-append (reverse chunks)))
        ((eq? (car args) 'immutable)
         (apply string-append (reverse chunks)))
        ((eq? (car args) 'mutable)
         (string-copy (apply string-append (reverse chunks))))
        ((eq? (car args) 'empty?)
         (= count 0))
        ((eq? (car args) 'count)
         count)
        ((eq? (car args) 'reset!)
         (set! chunks '())
         (set! count 0))
        ((char? (car args))
         (let ((piece (string (car args))))
           (set! chunks (cons piece chunks))
           (set! count (+ count 1))))
        ((string? (car args))
         (set! chunks (cons (car args) chunks))
         (set! count (+ count (string-length (car args)))))
        (else
         (error "string-builder: expected character, string, or command"
                (car args)))))))

(define string-slice substring)

(define (keyword-arg args key default)
  (let loop ((xs args))
    (cond ((null? xs) default)
          ((null? (cdr xs)) (error "keyword argument missing value" key))
          ((eq? (car xs) key) (cadr xs))
          (else (loop (cddr xs))))))

; string-join - join strings with an optional separator/prefix/suffix.
; Each argument is picked up positionally (like the other optional-arg
; wrappers in this file), so 1 or 2 optional args apply just that many
; instead of silently discarding a 2-arg (sep, prefix) call.
(define (string-join strings . args)
  (let* ((infix (if (pair? args) (car args) " "))
         (rest1 (if (pair? args) (cdr args) '()))
         (prefix (if (pair? rest1) (car rest1) ""))
         (rest2 (if (pair? rest1) (cdr rest1) '()))
         (suffix (if (pair? rest2) (car rest2) "")))
    (let loop ((xs strings) (first? #t) (out prefix))
      (cond ((null? xs) (string-append out suffix))
            (first?
             (loop (cdr xs) #f (string-append out (car xs))))
            (else
             (loop (cdr xs) #f (string-append out infix (car xs))))))))

(define (string-joiner . args)
  (let ((infix (keyword-arg args 'infix ""))
        (prefix (keyword-arg args 'prefix ""))
        (suffix (keyword-arg args 'suffix "")))
    (lambda strings
      (string-join strings infix prefix suffix))))

(define (string-joiner* . args)
  (let ((joiner (apply string-joiner args)))
    (lambda (strings)
      (apply joiner strings))))

(define (delimiter-procedure delimiter)
  (cond ((procedure? delimiter) delimiter)
        ((char? delimiter) (lambda (ch) (char=? ch delimiter)))
        (else (error "string-splitter: unsupported delimiter" delimiter))))

(define (string-splitter . args)
  (let ((delimiter (delimiter-procedure
                     (keyword-arg args 'delimiter char-whitespace?)))
        (allow-runs? (keyword-arg args 'allow-runs? #t))
        (copier (keyword-arg args 'copier substring)))
    (lambda (str)
      (let ((len (string-length str)))
        (let loop ((i 0) (start 0) (parts '()))
          (cond
            ((>= i len)
             (reverse (if (and allow-runs? (= start i))
                          parts
                          (cons (copier str start i) parts))))
            ((delimiter (string-ref str i))
             (let skip ((j (+ i 1)))
               (if (and allow-runs?
                        (< j len)
                        (delimiter (string-ref str j)))
                   (skip (+ j 1))
                   (loop j j
                         (if (and allow-runs? (= start i))
                             parts
                             (cons (copier str start i) parts))))))
            (else
             (loop (+ i 1) start parts))))))))

;; Repeats via one string-append per remaining copy, each on a string that
;; has grown by one more `piece`, is O(count^2 * (string-length piece))
;; overall - noticeable once count reaches the tens of thousands (e.g.
;; string-pad-left/right on a large target width). Halving instead gives
;; O(log count) appends, each on progressively doubling strings, for
;; O(count * (string-length piece)) total work.
(define (repeat-string piece count)
  (cond
    ((<= count 0) "")
    ((= count 1) piece)
    (else
     (let ((half (repeat-string piece (quotient count 2))))
       (if (even? count)
           (string-append half half)
           (string-append half half piece))))))

(define (string-padder . args)
  (let ((where (keyword-arg args 'where 'leading))
        (fill-with (keyword-arg args 'fill-with " "))
        (clip? (keyword-arg args 'clip? #t)))
    (lambda (str len)
      (let ((n (string-length str)))
        (cond
          ((< n len)
           (let ((padding (repeat-string fill-with (- len n))))
             (if (eq? where 'trailing)
                 (string-append str padding)
                 (string-append padding str))))
          ((and clip? (> n len))
           (if (eq? where 'trailing)
               (substring str 0 len)
               (substring str (- n len) n)))
          (else str))))))

(define (string-pad-left str k . maybe-char)
  ((string-padder 'where 'leading
                  'fill-with (string (if (null? maybe-char)
                                         #\space
                                         (car maybe-char))))
   str
   k))

(define (string-pad-right str k . maybe-char)
  ((string-padder 'where 'trailing
                  'fill-with (string (if (null? maybe-char)
                                         #\space
                                         (car maybe-char))))
   str
   k))

(define (trim-procedure to-trim)
  (cond ((procedure? to-trim) to-trim)
        ((char? to-trim) (lambda (ch) (char=? ch to-trim)))
        (else (error "string-trimmer: unsupported trim predicate" to-trim))))

(define (string-trimmer . args)
  (let ((where (keyword-arg args 'where 'both))
        (to-trim (trim-procedure (keyword-arg args 'to-trim char-whitespace?)))
        (copier (keyword-arg args 'copier substring)))
    (lambda (str)
      (let* ((len (string-length str))
             (start (if (or (eq? where 'leading) (eq? where 'both))
                        (let loop ((i 0))
                          (if (and (< i len) (to-trim (string-ref str i)))
                              (loop (+ i 1))
                              i))
                        0))
             (end (if (or (eq? where 'trailing) (eq? where 'both))
                      (let loop ((i (- len 1)))
                        (if (and (>= i start) (to-trim (string-ref str i)))
                            (loop (- i 1))
                            (+ i 1)))
                      len)))
        (copier str start end)))))

(define (string-replace str char1 char2)
  (list->string
    (map (lambda (ch) (if (char=? ch char1) char2 ch))
         (string->list str))))

(define (string-copy! to at from . rest)
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (string-length from)))
         (temp (string-copy from start end)))
    (let loop ((i 0) (j at))
      (if (= i (string-length temp))
          j
          (begin
            (string-set! to j (string-ref temp i))
            (loop (+ i 1) (+ j 1)))))))

;;; ============================================================================
;;; Additional R7RS compatibility procedures
;;; ============================================================================

(define (boolean=? first second . rest)
  (and (boolean? first)
       (boolean? second)
       (eq? first second)
       (let loop ((xs rest))
         (or (null? xs)
             (and (boolean? (car xs))
                  (eq? first (car xs))
                  (loop (cdr xs)))))))

(define (call-with-port port proc)
  (dynamic-wind
    (lambda () #f)
    (lambda () (proc port))
    (lambda () (close-port port))))

(define (close-port port)
  (cond
    ((input-port? port) (close-input-port port))
    ((output-port? port) (close-output-port port))
    (else (error "close-port: not a port"))))

(define exact inexact->exact)
(define inexact exact->inexact)

(define (exact-integer? x)
  (and (integer? x) (exact? x)))

(define (exact-integer-sqrt n)
  (if (not (exact-nonnegative-integer? n))
      (error "exact-integer-sqrt: expected exact nonnegative integer" n)
      (let loop ((lo 0) (hi (+ n 1)))
        (if (<= (- hi lo) 1)
            (values lo (- n (* lo lo)))
            (let* ((mid (quotient (+ lo hi) 2))
                   (sq (* mid mid)))
              (if (<= sq n)
                  (loop mid hi)
                  (loop lo mid)))))))

(define (exact-nonnegative-integer? x)
  (and (exact-integer? x) (>= x 0)))

(define (exact-rational? x)
  (and (rational? x) (exact? x)))

(define (1+ z) (+ z 1))
(define (-1+ z) (- z 1))

(define (copysign x y)
  (let ((mag (abs x)))
    (if (or (< y 0)
            (and (= y 0) (< (/ 1.0 y) 0)))
        (- mag)
        mag)))

(define (integer-select divproc n d selector)
  (call-with-values
    (lambda () (divproc n d))
    selector))

(define (euclidean/ n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r)
      (cond
        ((< r 0)
         (if (> d 0)
             (values (- q 1) (+ r d))
             (values (+ q 1) (- r d))))
        (else (values q r))))))

(define (ceiling/ n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r)
      (if (and (not (= r 0))
               (or (and (> n 0) (> d 0))
                   (and (< n 0) (< d 0))))
          (values (+ q 1) (- r d))
          (values q r)))))

(define (round/ n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r)
      (let* ((abs-r (abs r))
             (abs-d (abs d))
             (twice-r (* 2 abs-r))
             (same-sign? (or (and (> n 0) (> d 0))
                             (and (< n 0) (< d 0))))
             (adjust? (or (> twice-r abs-d)
                          (and (= twice-r abs-d) (odd? q)))))
        (if adjust?
            (if same-sign?
                (values (+ q 1) (- r d))
                (values (- q 1) (+ r d)))
            (values q r))))))

(define (euclidean-quotient n d)
  (integer-select euclidean/ n d (lambda (q r) q)))

(define (euclidean-remainder n d)
  (integer-select euclidean/ n d (lambda (q r) r)))

(define (floor-quotient n d)
  (call-with-values
    (lambda () (floor/ n d))
    (lambda (q r) q)))

(define (floor-remainder n d)
  (call-with-values
    (lambda () (floor/ n d))
    (lambda (q r) r)))

(define (ceiling-quotient n d)
  (integer-select ceiling/ n d (lambda (q r) q)))

(define (ceiling-remainder n d)
  (integer-select ceiling/ n d (lambda (q r) r)))

(define (truncate-quotient n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r) q)))

(define (truncate-remainder n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r) r)))

(define (round-quotient n d)
  (integer-select round/ n d (lambda (q r) q)))

(define (round-remainder n d)
  (integer-select round/ n d (lambda (q r) r)))

(define integer-floor floor-quotient)
(define integer-ceiling ceiling-quotient)
(define integer-truncate truncate-quotient)
(define integer-round round-quotient)

(define (integer-divide n d)
  (call-with-values
    (lambda () (truncate/ n d))
    (lambda (q r) (vector q r))))

(define (integer-divide-quotient qr)
  (vector-ref qr 0))

(define (integer-divide-remainder qr)
  (vector-ref qr 1))

(define (modexp b e m)
  (if (= m 0)
      (error "modexp: modulus must be nonzero")
      (let loop ((base (modulo b m))
                 (exp e)
                 (result (modulo 1 m)))
        (cond
          ((< exp 0) (error "modexp: negative exponent"))
          ((= exp 0) result)
          ((odd? exp)
           (loop (modulo (* base base) m)
                 (quotient exp 2)
                 (modulo (* result base) m)))
          (else
           (loop (modulo (* base base) m)
                 (quotient exp 2)
                 result))))))

(define (floor->exact x) (exact (floor x)))
(define (ceiling->exact x) (exact (ceiling x)))
(define (truncate->exact x) (exact (truncate x)))
(define (round->exact x) (exact (round x)))

(define (rationalize->exact x y)
  (exact (rationalize x y)))

(define (simplest-rational x y)
  (rationalize (/ (+ x y) 2) (/ (abs (- y x)) 2)))

(define (simplest-exact-rational x y)
  (exact (simplest-rational x y)))

(define logp1 log1p)
(define (exp2 z) (expt 2 z))
(define (exp10 z) (expt 10 z))
(define (exp2m1 z) (expm1 (* z (log 2))))
(define (exp10m1 z) (expm1 (* z (log 10))))
(define (log2 z) (/ (log z) (log 2)))
(define (log10 z) (/ (log z) (log 10)))
(define (log2p1 z) (/ (log1p z) (log 2)))
(define (log10p1 z) (/ (log1p z) (log 10)))
(define (log1mexp x)
  (if (<= x (- (log 2)))
      (log1p (- (exp x)))
      (log (- (expm1 x)))))

(define (versin z) (- 1 (cos z)))
(define (exsec z) (/ (versin z) (cos z)))
(define (aversin z) (acos (- 1 z)))
(define (aexsec z) (acos (/ 1 (+ 1 z))))

(define (logistic x)
  (if (< x 0)
      (let ((e (exp x))) (/ e (+ 1 e)))
      (/ 1 (+ 1 (exp (- x))))))

(define (logit p)
  (log (/ p (- 1 p))))

(define (logistic-1/2 x)
  (- (logistic x) 1/2))

(define (logit1/2+ p)
  (logit (+ 1/2 p)))

(define (log-logistic x)
  (- (log1pexp (- x))))

(define (logit-exp x)
  (- x (log1mexp x)))

(define (logsumexp xs)
  (if (null? xs)
      (log 0)
      (let ((m (apply max xs)))
        (if (infinite? m)
            m
            (+ m (log (apply + (map (lambda (x) (exp (- x m))) xs))))))))

(define pi (acos -1))
(define (sin-pi* x) (sin (* pi x)))
(define (cos-pi* x) (cos (* pi x)))
(define (tan-pi* x) (tan (* pi x)))
(define (versin-pi* x) (versin (* pi x)))
(define (exsec-pi* x) (exsec (* pi x)))
(define (asin/pi x) (/ (asin x) pi))
(define (acos/pi x) (/ (acos x) pi))
(define (atan/pi x) (/ (atan x) pi))
(define (atan2/pi y x) (/ (atan y x) pi))
(define (aversin/pi x) (/ (aversin x) pi))
(define (aexsec/pi x) (/ (aexsec x) pi))

(define (rsqrt z) (/ 1 (sqrt z)))
(define (compound z1 z2) (expt (+ 1 z1) z2))
(define (compoundm1 z1 z2) (expm1 (* z2 (log1p z1))))
(define (conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

(define (digit-value ch)
  (let ((n (char->integer ch)))
    (cond
      ((and (>= n (char->integer #\0)) (<= n (char->integer #\9)))
       (- n (char->integer #\0)))
      ((and (>= n (char->integer #\A)) (<= n (char->integer #\Z)))
       (+ 10 (- n (char->integer #\A))))
      ((and (>= n (char->integer #\a)) (<= n (char->integer #\z)))
       (+ 10 (- n (char->integer #\a))))
      ((and (>= n 1632) (<= n 1641)) (- n 1632))
      ((and (>= n 1776) (<= n 1785)) (- n 1776))
      ((and (>= n 2406) (<= n 2415)) (- n 2406))
      ((and (>= n 2534) (<= n 2543)) (- n 2534))
      ((and (>= n 2662) (<= n 2671)) (- n 2662))
      ((and (>= n 2790) (<= n 2799)) (- n 2790))
      ((and (>= n 2918) (<= n 2927)) (- n 2918))
      ((and (>= n 3046) (<= n 3055)) (- n 3046))
      ((and (>= n 3174) (<= n 3183)) (- n 3174))
      ((and (>= n 3302) (<= n 3311)) (- n 3302))
      ((and (>= n 3430) (<= n 3439)) (- n 3430))
      ((and (>= n 3664) (<= n 3673)) (- n 3664))
      ((and (>= n 3792) (<= n 3801)) (- n 3792))
      ((and (>= n 3872) (<= n 3881)) (- n 3872))
      ((and (>= n 4160) (<= n 4169)) (- n 4160))
      ((and (>= n 6112) (<= n 6121)) (- n 6112))
      ((and (>= n 6160) (<= n 6169)) (- n 6160))
      ((and (>= n 6470) (<= n 6479)) (- n 6470))
      ((and (>= n 6608) (<= n 6617)) (- n 6608))
      ((and (>= n 6672) (<= n 6681)) (- n 6672))
      ((and (>= n 6784) (<= n 6793)) (- n 6784))
      ((and (>= n 6800) (<= n 6809)) (- n 6800))
      ((and (>= n 6992) (<= n 7001)) (- n 6992))
      ((and (>= n 7088) (<= n 7097)) (- n 7088))
      ((and (>= n 7232) (<= n 7241)) (- n 7232))
      ((and (>= n 7248) (<= n 7257)) (- n 7248))
      ((and (>= n 42528) (<= n 42537)) (- n 42528))
      ((and (>= n 43216) (<= n 43225)) (- n 43216))
      ((and (>= n 43264) (<= n 43273)) (- n 43264))
      ((and (>= n 43472) (<= n 43481)) (- n 43472))
      ((and (>= n 43504) (<= n 43513)) (- n 43504))
      ((and (>= n 43600) (<= n 43609)) (- n 43600))
      ((and (>= n 44016) (<= n 44025)) (- n 44016))
      ((and (>= n 65296) (<= n 65305)) (- n 65296))
      (else #f))))

;;; ============================================================================
;;; Pathname/File Utilities
;;; ============================================================================

(define (path-separator) "/")

(define (path-absolute? path)
  (and (> (string-length path) 0)
       (char=? (string-ref path 0) #\/)))

(define (path-strip-trailing-separators path)
  (let loop ((end (string-length path)))
    (if (and (> end 1) (char=? (string-ref path (- end 1)) #\/))
        (loop (- end 1))
        (substring path 0 end))))

(define (path-join . parts)
  (define (empty-part? s) (= (string-length s) 0))
  (define (trim-left s)
    (let loop ((i 0))
      (if (and (< i (string-length s)) (char=? (string-ref s i) #\/))
          (loop (+ i 1))
          (substring s i (string-length s)))))
  (define (trim-right s)
    (path-strip-trailing-separators s))
  (let loop ((parts parts) (out ""))
    (cond ((null? parts) (if (string=? out "") "." out))
          ((empty-part? (car parts)) (loop (cdr parts) out))
          ((or (string=? out "") (path-absolute? (car parts)))
           (loop (cdr parts) (trim-right (car parts))))
          (else
           (let ((left (trim-right out))
                 (right (trim-left (car parts))))
             (loop (cdr parts)
                   (if (string=? left "/")
                       (string-append left right)
                       (string-append left "/" right))))))))

(define (path-basename path)
  (let* ((p (path-strip-trailing-separators path))
         (len (string-length p)))
    (let loop ((i (- len 1)))
      (cond ((< i 0) p)
            ((char=? (string-ref p i) #\/) (substring p (+ i 1) len))
            (else (loop (- i 1)))))))

(define (path-directory path)
  (let* ((p (path-strip-trailing-separators path))
         (len (string-length p)))
    (let loop ((i (- len 1)))
      (cond ((< i 0) ".")
            ((= i 0) "/")
            ((char=? (string-ref p i) #\/) (substring p 0 i))
            (else (loop (- i 1)))))))

(define (path-extension path)
  (let ((base (path-basename path)))
    (let loop ((i (- (string-length base) 1)))
      (cond ((<= i 0) "")
            ((char=? (string-ref base i) #\.) (substring base (+ i 1) (string-length base)))
            (else (loop (- i 1)))))))

(define (path-with-extension path extension)
  (let* ((base (path-basename path))
         (dir (path-directory path))
         (stem
          (let loop ((i (- (string-length base) 1)))
            (cond ((<= i 0) base)
                  ((char=? (string-ref base i) #\.) (substring base 0 i))
                  (else (loop (- i 1)))))))
    (path-join dir
               (string-append stem
                              (if (and (> (string-length extension) 0)
                                       (not (char=? (string-ref extension 0) #\.)))
                                  "."
                                  "")
                              extension))))

(define *supported-r7rs-environments*
  '((scheme base)
    (scheme case-lambda)
    (scheme char)
    (scheme complex)
    (scheme cxr)
    (scheme eval)
    (scheme file)
    (scheme inexact)
    (scheme lazy)
    (scheme load)
    (scheme process-context)
    (scheme read)
    (scheme repl)
    (scheme sort)
    (scheme time)
    (scheme write)))

(define (supported-r7rs-environment? spec)
  (if (member spec *supported-r7rs-environments*) #t #f))

(define (environment . specs)
  (for-each
    (lambda (spec)
      (if (not (supported-r7rs-environment? spec))
          (error "environment: unsupported library specifier" spec)))
    specs)
  (interaction-environment))

(define-syntax import
  (syntax-rules ()
    ((import spec ...)
     (begin))))

(define-syntax define-library
  (syntax-rules (export import begin include include-ci)
    ((define-library name)
     (begin))
    ((define-library name (export export-spec ...) clause ...)
     (define-library name clause ...))
    ((define-library name (import import-spec ...) clause ...)
     (define-library name clause ...))
    ((define-library name (begin body ...) clause ...)
     (begin body ... (define-library name clause ...)))
    ((define-library name (include filename ...) clause ...)
     (begin (include filename ...) (define-library name clause ...)))
    ((define-library name (include-ci filename ...) clause ...)
     (begin (include-ci filename ...) (define-library name clause ...)))))

(define-syntax include
  (syntax-rules ()
    ((include filename ...)
     (begin (load filename) ...))))

(define-syntax include-ci
  (syntax-rules ()
    ((include-ci filename ...)
     (include filename ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var init) ...) body ...)
     (let ((var #f) ...)
       (set! var init) ...
       body ...))))

(define (list-set! lst k obj)
  (set-car! (list-tail lst k) obj))

(define (port? obj)
  (or (input-port? obj) (output-port? obj)))

(define (square x)
  (* x x))

(define (string->vector str . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (string-length str)
                  (cadr rest)))
         (vec (make-vector (- end start))))
    (let loop ((i start) (j 0))
      (if (< i end)
          (begin
            (vector-set! vec j (string-ref str i))
            (loop (+ i 1) (+ j 1)))
          vec))))

(define (symbol=? first second . rest)
  (and (symbol? first)
       (symbol? second)
       (eq? first second)
       (let loop ((xs rest))
         (or (null? xs)
             (and (symbol? (car xs))
                  (eq? first (car xs))
                  (loop (cdr xs)))))))

(define (vector->string vec . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (vector-length vec)
                  (cadr rest)))
         (str (make-string (- end start))))
    (let loop ((i start) (j 0))
      (if (< i end)
          (begin
            (string-set! str j (vector-ref vec i))
            (loop (+ i 1) (+ j 1)))
          str))))

;;; ============================================================================
;;; SRFI-2: and-let* (guarded evaluation)
;;; ============================================================================

;; (and-let* ((var expr) ...) body ...)
;; Sequential binding with short-circuit on #f
(define-syntax and-let*
  (syntax-rules ()
    ;; No bindings, just body
    ((and-let* () body ...)
     (begin body ...))
    ;; No bindings, no body -> #t
    ((and-let* ())
     #t)
    ;; Binding without variable (just test)
    ((and-let* ((expr) more ...) body ...)
     (if expr
         (and-let* (more ...) body ...)
         #f))
    ;; Binding with variable
    ((and-let* ((var expr) more ...) body ...)
     (let ((var expr))
       (if var
           (and-let* (more ...) body ...)
           #f)))))
