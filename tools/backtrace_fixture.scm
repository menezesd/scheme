; Fixture for `make test-backtrace`: an unhandled error prints a backtrace
; after the report; handled ones print nothing. Line numbers are part of the
; expected output in the Makefile, so edit both together.
(define (bt-leaf x) (+ 1 (car x)))
(define (bt-middle x)
  (* 2 (bt-leaf x)))
(define (bt-recur n)
  (if (= n 0) (bt-middle 5) (+ 1 (bt-recur (- n 1)))))
(display (guard (e (#t 'caught)) (bt-recur 3)))
(newline)
(display (with-exception-handler
          (lambda (e) 10)
          (lambda () (+ 1 (raise-continuable 'oops)))))
(newline)
(bt-recur 50)
(display "not reached")
