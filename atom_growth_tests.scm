;;; Atom-table growth under keyword shadowing.
;;;
;;; The atom table has a fixed size and the process panics with "atom table
;;; full" once it is exhausted. The CPS interpreter expands an inner lambda's
;;; body every time the lambda is made, so a macro used there is expanded
;;; again on every call. If an expansion minted a symbol - as renaming a
;;; template's if past a use site that rebinds if once did, two per expansion
;;; here - the million expansions below (500000 calls of each procedure) would
;;; overrun the table. Core keywords are renamed to their permanent core
;;; aliases instead. Run by `make test-atom-growth` in both engines.

(define-syntax my-when
  (syntax-rules () ((_ c e) (if c (begin e) #f))))

(define (shadowed-user-macro n)
  (let ((if list) (begin list))
    ((lambda () (my-when #t n)))))

(define (shadowed-stdlib-macro n)
  (let ((if list) (begin list))
    ((lambda () (when #t n)))))

(define (gensym-counter)
  (let ((s (symbol->string (gensym))))
    (string->number (substring s 1 (string-length s)))))

(define (run i acc)
  (if (= i 500000)
      acc
      (run (+ i 1) (+ acc (shadowed-user-macro 1) (shadowed-stdlib-macro 1)))))

(define before (gensym-counter))
(define result (run 0 0))
(define minted (- (gensym-counter) before 1))

(display "atom growth: ")
(display (if (and (= result 1000000) (< minted 100))
             "all passed"
             (list 'failed result minted)))
(newline)
