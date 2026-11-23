; Fixture for `make test-backtrace`: a deep mutual recursion, whose
; backtrace must stop at the line budget with a count of the frames left out.
(define (bt-even n) (if (= n 0) (vector-ref (vector) 0) (+ 1 (bt-odd (- n 1)))))
(define (bt-odd n) (+ 1 (bt-even (- n 1))))
(bt-even 5000)
