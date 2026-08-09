; Test Suite for Scheme Interpreter
; Run with: ./lisp < test.scm

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "  FAIL: ")
        (display name)
        (newline)
        (display "    expected: ")
        (write expected)
        (newline)
        (display "    got:      ")
        (write actual)
        (newline))))

(define (section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ============================================================================
;;; Arithmetic
;;; ============================================================================

(section "Arithmetic")

(test "addition" 6 (+ 1 2 3))
(test "subtraction" 5 (- 10 3 2))
(test "multiplication" 24 (* 2 3 4))
(test "division" 4 (/ 12 3))
; Note: (/ 1 2) returns the integer 0 in integer division, not 1/2
(test "division to rational" 1/3 (/ 1 3))
(test "modulo" 1 (modulo 10 3))
(test "remainder" 1 (remainder 10 3))
(test "quotient" 3 (quotient 10 3))
(test "min preserves NaN and signed-zero operand order"
      '(#t #f #t #f)
      (list (nan? (min +nan.0 1.0))
            (nan? (min 1.0 +nan.0))
            (negative? (/ 1.0 (min -0.0 0.0)))
            (negative? (/ 1.0 (min 0.0 -0.0)))))
(test "abs positive" 5 (abs 5))
(test "abs negative" 5 (abs -5))
(test "gcd" 6 (gcd 12 18))
(test "lcm" 36 (lcm 12 18))

;;; ============================================================================
;;; Numeric predicates
;;; ============================================================================

(section "Numeric Predicates")

(test "zero? true" #t (zero? 0))
(test "zero? false" #f (zero? 1))
(test "positive? true" #t (positive? 5))
(test "positive? false" #f (positive? -5))
(test "negative? true" #t (negative? -5))
(test "negative? false" #f (negative? 5))
(test "odd? true" #t (odd? 3))
(test "odd? false" #f (odd? 4))
(test "even? true" #t (even? 4))
(test "even? false" #f (even? 3))

;;; ============================================================================
;;; Comparisons
;;; ============================================================================

(section "Comparisons")

(test "< true" #t (< 1 2 3))
(test "< false" #f (< 1 3 2))
(test "> true" #t (> 3 2 1))
(test "<= true" #t (<= 1 2 2 3))
(test ">= true" #t (>= 3 2 2 1))
(test "= true" #t (= 2 2 2))
(test "= false" #f (= 2 2 3))

;;; ============================================================================
;;; List operations
;;; ============================================================================

(section "List Operations")

(test "cons" '(1 . 2) (cons 1 2))
(test "car" 1 (car '(1 2 3)))
(test "cdr" '(2 3) (cdr '(1 2 3)))
(test "list" '(1 2 3) (list 1 2 3))
(test "length" 3 (length '(1 2 3)))
(test "append" '(1 2 3 4) (append '(1 2) '(3 4)))
(test "reverse" '(3 2 1) (reverse '(1 2 3)))
(test "list-ref" 'b (list-ref '(a b c) 1))
(test "list-tail" '(c d) (list-tail '(a b c d) 2))
(test "list-ref rejects circular list"
      #t
      (let ((cycle (cons 'a '())))
        (set-cdr! cycle cycle)
        (guard (e (#t #t)) (list-ref cycle 0) #f)))
(test "list-tail rejects circular list"
      #t
      (let ((cycle (cons 'a '())))
        (set-cdr! cycle cycle)
        (guard (e (#t #t)) (list-tail cycle 0) #f)))

;;; ============================================================================
;;; Predicates
;;; ============================================================================

(section "Type Predicates")

(test "null? true" #t (null? '()))
(test "null? false" #f (null? '(1)))
(test "pair? true" #t (pair? '(1 . 2)))
(test "pair? false" #f (pair? 5))
(test "list? true" #t (list? '(1 2 3)))
(test "list? false" #f (list? '(1 . 2)))
(test "symbol? true" #t (symbol? 'foo))
(test "symbol? false" #f (symbol? 5))
(test "number? true" #t (number? 42))
(test "string? true" #t (string? "hello"))
(test "char? true" #t (char? #\a))
(test "vector? true" #t (vector? #(1 2 3)))
(test "procedure? true" #t (procedure? +))
(test "boolean? true" #t (boolean? #t))
(test "boolean? false" #f (boolean? 0))
(test "#true boolean alias" #t #true)
(test "#false boolean alias" #f #false)

;;; ============================================================================
;;; Equality
;;; ============================================================================

(section "Equality")

(test "eq? same symbol" #t (eq? 'a 'a))
(test "eq? different" #f (eq? 'a 'b))
(test "eqv? numbers" #t (eqv? 42 42))
(test "equal? lists" #t (equal? '(1 2 3) '(1 2 3)))
(test "equal? nested" #t (equal? '(1 (2 3)) '(1 (2 3))))

;;; ============================================================================
;;; Membership and association
;;; ============================================================================

(section "Membership and Association")

(test "memq found" '(b c) (memq 'b '(a b c)))
(test "memq not found" #f (memq 'd '(a b c)))
(test "member found" '((2) 3) (member '(2) '(1 (2) 3)))
(test "member-procedure" '((2) 3)
      ((member-procedure (lambda (a b) (equal? a b))) '(2) '(1 (2) 3)))
(test "assq found" '(b . 2) (assq 'b '((a . 1) (b . 2) (c . 3))))
(test "assq not found" #f (assq 'd '((a . 1) (b . 2))))
(test "assoc found" '((1) . a) (assoc '(1) '(((0) . z) ((1) . a))))
(test "association searches short-circuit dotted tails" '((1 . a) (1 . a))
      (list
       (assq 1 '((1 . a) . tail))
       (assoc 1 '((1 . a) . tail))))
(test "list comparator arguments short-circuit on empty lists"
      '(#f #f #t #t #t #f)
      (list
       (guard (e (#t #f)) (member 'x '() #f))
       (guard (e (#t #f)) (assoc 'x '() #f))
       (guard (e (#t #f)) (null? (delete 'x '() #f)))
       (guard (e (#t #f)) (null? (delete-duplicates '() #f)))
       (guard (e (#t #f)) (null? (lset-union #f)))
       (guard (e (#t #t)) (lset<= #f) #f)))
(test "list= skips validation with zero or one list" '(#t #t)
      (list (list= #f)
            (list= #f 42)))
(test "lset comparators are lazy on vacuous operations"
      '(#t #t () #f #t)
      (list (lset<= #f)
            (lset= #f)
            (lset-adjoin #f '())
            (lset<= #f '(1) '())
            (guard (e (#t #t)) (lset<= #f '(1) '(1)) #f)))
(test "alist? recognizes proper association lists" '(#t #t #f #f #f)
      (let ((cycle (cons (cons 'a 1) '())))
        (set-cdr! cycle cycle)
        (list (alist? '())
              (alist? '((a . 1) (b . 2)))
              (alist? '(a b))
              (alist? (cons '(a . 1) 'tail))
              (alist? cycle))))
(test "lset<= and lset=" '(#t #t)
      (list (lset<= eq? '(a) '(a b a) '(a b c c))
            (lset= eq? '(b e a) '(a e b) '(e e b a))))
(test "lset union/intersection/difference/xor"
      '((u o i a b c d e) (a e) (b c d) (d c b i o u))
      (list (lset-union eq? '(a b c d e) '(a e i o u))
            (lset-intersection eq? '(a b c d e) '(a e i o u))
            (lset-difference eq? '(a b c d e) '(a e i o u))
            (lset-xor eq? '(a b c d e) '(a e i o u))))
(test "lset-adjoin and diff+intersection"
      '((u o i a b c d c e) ((b c d) (a e)))
      (list (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)
            (call-with-values
              (lambda ()
                (lset-diff+intersection eq?
                                           '(a b c d e) '(a e i o u)))
              list)))
(test "lset operations reject non-finite operands" '(#t #t)
      (let ((cycle (circular-list 'a 'b)))
        (list
         (guard (e (#t #t)) (lset-union eq? '(a) cycle) #f)
         (guard (e (#t #t)) (lset-intersection eq? '(a) (cons 'a 'tail)) #f))))
(test "unzip procedures reject malformed list-of-lists" '(#t #t #t #t #t)
      (let ((cycle (list '(a b c d e) '(f g h i j))))
        (set-cdr! (cdr cycle) cycle)
        (list
         (guard (e (#t #t)) (unzip1 cycle) #f)
         (guard (e (#t #t)) (unzip2 cycle) #f)
         (guard (e (#t #t)) (unzip3 cycle) #f)
         (guard (e (#t #t)) (unzip4 cycle) #f)
         (guard (e (#t #t)) (unzip5 cycle) #f))))
(test "append-map rejects all-circular inputs" '(#t #t)
      (let ((cycle (circular-list 1 2)))
        (list
         (guard (e (#t #t)) (append-map list cycle) #f)
         (guard (e (#t #t)) (append-map! list cycle) #f))))

;;; ============================================================================
;;; Control flow
;;; ============================================================================

(section "Control Flow")

(test "if true" 1 (if #t 1 2))
(test "if false" 2 (if #f 1 2))
(test "cond" 'big (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'big)))
(test "and true" 3 (and 1 2 3))
(test "and false" #f (and 1 #f 3))
(test "or true" 1 (or #f 1 2))
(test "or false" #f (or #f #f #f))
(test "not true" #t (not #f))
(test "not false" #f (not #t))

;;; ============================================================================
;;; Let forms
;;; ============================================================================

(section "Let Forms")

(test "let" 3 (let ((x 1) (y 2)) (+ x y)))
(test "let*" 3 (let* ((x 1) (y (+ x 1))) (+ x y)))
(test "letrec" 120 (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5)))
(test "named let" 55 (let loop ((n 10) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n)))))

;;; ============================================================================
;;; Lambda and closures
;;; ============================================================================

(section "Lambda and Closures")

(test "lambda" 5 ((lambda (x) (+ x 2)) 3))
(test "closure" 10 (let ((x 5)) ((lambda (y) (+ x y)) 5)))
(test "higher order" 9 ((lambda (f) (f 3)) (lambda (x) (* x x))))
(test "varargs" '(1 2 3) ((lambda args args) 1 2 3))
(test "rest args" '(2 3) ((lambda (x . rest) rest) 1 2 3))

;;; ============================================================================
;;; Strings
;;; ============================================================================

(section "Strings")

(test "string-length" 5 (string-length "hello"))
(test "string-ref" #\e (string-ref "hello" 1))
(test "string-append" "helloworld" (string-append "hello" "world"))
(test "substring" "ell" (substring "hello" 1 4))
(test "substring returns immutable string" #t
      (guard (e (#t #t))
        (let ((s (substring (string-copy "hello") 1 4)))
          (string-set! s 0 #\E)
          #f)))
(test "string-append returns immutable string" #t
      (guard (e (#t #t))
        (let ((s (string-append "a" "b")))
          (string-set! s 0 #\A)
          #f)))
(test "list->string returns immutable string" #t
      (guard (e (#t #t))
        (let ((s (list->string '(#\a #\b))))
          (string-set! s 0 #\A)
          #f)))
(test "string-copy of substring remains mutable" "Ell"
      (let ((s (string-copy (substring "hello" 1 4))))
        (string-set! s 0 #\E)
        s))
(test "utf8->string returns immutable string" #t
      (let ((s (utf8->string #u8(97))))
        (guard (e (#t #t))
          (string-set! s 0 #\x)
          #f)))
(test "standard string producers are immutable" '(#t #t #t)
      (list
       (guard (e (#t #t))
         (let ((s (symbol->string 'foo))) (string-set! s 0 #\x) #f))
       (guard (e (#t #t))
         (let ((s (number->string 12))) (string-set! s 0 #\x) #f))
       (guard (e (#t #t))
         (let ((s (write-to-string 12))) (string-set! s 0 #\x) #f))))
(test "string->list" '(#\a #\b #\c) (string->list "abc"))
(test "list->string" "abc" (list->string '(#\a #\b #\c)))
(test "string->symbol" 'hello (string->symbol "hello"))
(test "symbol->string" "hello" (symbol->string 'hello))
(test "string<? multiple" #t (string<? "a" "b" "c"))
(test "string-ci=? unicode foldcase" #t (string-ci=? "straße" "STRASSE"))
(test "search and byte I/O validate arguments"
      '(#t #t #t #t #t)
      (list
       (guard (e (#t #t)) (string-search-forward "a" "abc" -1) #f)
       (guard (e (#t #t)) (string-search-forward "a" "abc" 2 1) #f)
       (guard (e (#t #t)) (string-search-backward "a" "abc" -1) #f)
       (guard (e (#t #t)) (string-search-backward "a" "abc" 0 4) #f)
       (guard (e (#t #t)) (write-u8 256 (open-output-string)) #f)))
(test "string search bounds and result" '(2 7 #f)
      (list
       (string-search-forward "rat" "pirate rating")
       (string-search-backward "rat" "pirate rating")
       (string-search-forward "rat" "pirate rating" 9 13)))
(test "MIT string search additions" '(#t #t #t #t)
      (list
       (equal? '(0 1 2) (string-search-all "aa" "aaaa"))
       (substring? "" "abc")
       (string-prefix-ci? "ab" "ABc")
       (string-suffix-ci? "BC" "abc")))
(test "string match counts" '(2 3)
      (list (string-match-forward "mirror" "micro")
            (string-match-backward "bulbous" "fractious")))
(test "character search" '(1 3 1 1)
      (list
       (string-find-next-char "abcba" #\b)
       (string-find-previous-char "abcba" #\b)
       (string-find-next-char-ci "aBc" #\b)
       (string-find-previous-char-ci "aBc" #\b)))
(test "character-set search" '(1 2 2 #f)
      (let ((set (char-set #\a #\b)))
        (list
         (string-find-next-char-in-set "cba" set)
         (string-find-previous-char-in-set "cba" set)
         (string-find-next-char-in-set "xxab" set 1 4)
         (string-find-previous-char-in-set "xyz" set))))
(test "character-set search validates arguments" '(#t #t #t)
      (list
       (guard (e (#t #t))
         (string-find-next-char-in-set "abc" #\a)
         #f)
       (guard (e (#t #t))
         (string-find-previous-char-in-set "abc" (char-set #\a) 2 1)
         #f)
       (guard (e (#t #t))
         (string-find-next-char-in-set "abc" (char-set #\a) 0 4)
         #f)))
(test "SRFI-140 predicate string search" '(2 4 2 4)
      (list
       (string-index "  abc " char-alphabetic?)
       (string-index-right "  abc " char-alphabetic?)
       (string-skip "  abc " char-whitespace?)
       (string-skip-right "  abc " char-whitespace?)))
(test "SRFI-140 predicate string search validates arguments" '(#t #t #t)
      (list
       (guard (e (#t #t)) (string-index "abc" #\a) #f)
       (guard (e (#t #t)) (string-skip "abc" char-alphabetic? 3 2) #f)
       (guard (e (#t #t)) (string-skip-right "abc" char-alphabetic? 0 4) #f)))
(test "SRFI-140 string slicing and contains" '("ab" "cdef" "ef" "abcd" 2 5)
      (list
       (string-take "abcdef" 2)
       (string-drop "abcdef" 2)
       (string-take-right "abcdef" 2)
       (string-drop-right "abcdef" 2)
       (string-contains "xxabcabc" "abc")
       (string-contains-right "xxabcabc" "abc")))
(test "SRFI-140 contains ranges and empty needle" '(5 2 1 3)
      (list
       (string-contains "xxabcabc" "abc" 3 8)
       (string-contains-right "xxabcabc" "abc" 0 7)
       (string-contains "abc" "" 1 3 0 0)
       (string-contains-right "abc" "" 1 3 0 0)))
(test "SRFI-140 slicing and contains validate arguments" '(#t #t #t #t)
      (list
       (guard (e (#t #t)) (string-take "abc" -1) #f)
       (guard (e (#t #t)) (string-drop "abc" 4) #f)
       (guard (e (#t #t)) (string-contains "abc" "b" 2 1) #f)
       (guard (e (#t #t)) (string-contains "abc" "b" 0 3 0 2) #f)))
(test "predicate string search" '(1 2)
      (list
       (string-find-first-index (lambda (c) (char=? c #\b)) "abcde")
       (string-find-last-index
        (lambda (a b) (char=? a b)) "abc" "xbc")))

;;; ============================================================================
;;; Characters
;;; ============================================================================

(section "Characters")

(test "char->integer" 97 (char->integer #\a))
(test "integer->char" #\a (integer->char 97))
(test "char<?" #t (char<? #\a #\b))
(test "char<? multiple" #t (char<? #\a #\b #\c))
(test "char-alphabetic?" #t (char-alphabetic? #\a))
(test "char-numeric?" #t (char-numeric? #\5))
(test "char-whitespace?" #t (char-whitespace? #\space))
(test "char-upcase" #\A (char-upcase #\a))
(test "char-downcase" #\a (char-downcase #\A))
(test "bit-set? validates bit positions" '(#t #t #t)
      (list
       (bit-set? 3 8)
       (guard (e (#t #t)) (bit-set? -1 1) #f)
       (guard (e (#t #t)) (bit-set? 1.0 1) #f)))
(test "MIT integer bit operations" '(8 28 10 7 10 1 #t 3 7 8)
      (list (bit 3)
            (bits 4 2)
            (set-bit 1 8)
            (clear-bit 3 15)
            (toggle-bit 1 8)
            (extract-bit 3 8)
            (bit-clear? 1 8)
            (first-set-bit 40)
            (integer-length 127)
            (integer-length -129)))
(test "MIT complemented bit operations" '(2 0 -1 -3 -2 -4 -3 -1 8 1 0 6)
      (list (bitwise-andc1 1 3)
            (bitwise-andc2 1 3)
            (bitwise-orc1 1 3)
            (bitwise-orc2 1 3)
            (bitwise-nand 1 3)
            (bitwise-nor 1 3)
            (bitwise-eqv 1 3)
            (bitwise-eqv)
            (shift-left 1 3)
            (shift-right 8 3)
            (bit-mask 0 4)
            (bit-mask 2 1)))
(test "MIT bit anti-mask" -7 (bit-antimask 2 1))
(test "MIT variadic bitwise identities" '(-1 0 0)
      (list (bitwise-and) (bitwise-ior) (bitwise-xor)))
(test "MIT bitwise operations accept large arity"
      '(-1 0 0)
      (list (apply bitwise-and (make-list 1000 -1))
            (apply bitwise-ior (make-list 1000 0))
            (apply bitwise-xor (make-list 1000 0))))
(test "MIT bitwise operations accept arbitrary-precision integers"
      (list 0 (+ (expt 2 70) 1) (- -1 (expt 2 70))
            (+ (expt 2 70) (expt 2 69))
            (expt 2 70) -1 -1 0 0)
      (list (bitwise-and (expt 2 70) 1)
            (bitwise-ior (expt 2 70) 1)
            (bitwise-not (expt 2 70))
            (bitwise-xor (expt 2 70) (expt 2 69))
            (bitwise-and -1 (expt 2 70))
            (bitwise-ior -1 0)
            (bitwise-xor -1 0)
            (bitwise-not -1)
            (bitwise-and (- 0 (expt 2 70)) 1)))
(test "MIT arithmetic-shift accepts arbitrary-precision integers"
      (list 1024 -1024 -1 (* (expt 2 70) 32))
      (list (arithmetic-shift (expt 2 100) -90)
            (arithmetic-shift (- 0 (expt 2 100)) -90)
            (arithmetic-shift -1 -100)
            (arithmetic-shift (expt 2 70) 5)))
(test "arithmetic-shift saturates huge negative counts"
      '(0 -1)
      (list (arithmetic-shift 123 (- (expt 2 70)))
            (arithmetic-shift -123 (- (expt 2 70)))))

;;; ============================================================================
;;; Vectors
;;; ============================================================================

(section "Vectors")

(test "make-vector" #(0 0 0) (make-vector 3 0))
(test "vector" #(1 2 3) (vector 1 2 3))
(test "vector-length" 3 (vector-length #(1 2 3)))
(test "vector-ref" 2 (vector-ref #(1 2 3) 1))
(test "vector->list" '(1 2 3) (vector->list #(1 2 3)))
(test "list->vector" #(1 2 3) (list->vector '(1 2 3)))

;;; ============================================================================
;;; Higher order functions
;;; ============================================================================

(section "Higher Order Functions")

(test "map" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "map does not validate unused procedure on empty input" '()
      (map #f '()))
(test "map does not validate unused companion on empty input" '()
      (map #f '() 1))
(test "map-in-order" '(2 3 4)
      (map-in-order (lambda (x) (+ x 1)) '(1 2 3)))
(test "map finite plus circular list"
      '((1 a) (2 b) (3 a) (4 b) (5 a))
      (let ((c (circular-list 'a 'b)))
        (map (lambda (x y) (list x y)) '(1 2 3 4 5) c)))
(test "map rejects all circular lists" #t
      (guard (e (#t #t))
        (map list (circular-list 1 2) (circular-list 3 4))
        #f))
(test "for-each finite plus circular list" 5
      (let ((c (circular-list 'a 'b)) (n 0))
        (for-each (lambda (x y) (set! n (+ n 1))) '(1 2 3 4 5) c)
        n))
(test "for-each does not validate unused companion on empty input" 'ok
      (begin (for-each #f '() 1) 'ok))
(test "any/every/count finite plus circular" '(#t #t 3)
      (let ((c (circular-list 'a 'b)))
        (list (any eq? '(x y a) c)
              (every < '(0 1 0) (circular-list 2 3))
              (count eq? '(a b a c) c))))
(test "any/every/count skip unused companions on empty input"
      '(#f #t 0)
      (list (any #f '() 1)
            (every #f '() 1)
            (count #f '() 1)))
(test "SRFI-1 traversals skip unused companions on empty input"
      '(() () () ok 9 9)
      (list (append-map #f '() 1)
            (append-map! #f '() 1)
            (filter-map #f '() 1)
            (begin (pair-for-each #f '() 1) 'ok)
            (pair-fold #f 9 '() 1)
            (pair-fold-right #f 9 '() 1)))
(test "fold finite plus circular" 46
      (fold + 0 '(1 2 3) (circular-list 10 20)))
(test "apply" 6 (apply + '(1 2 3)))
(test "for-each" 6 (let ((sum 0)) (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3)) sum))
(test "for-each multiple stops at shortest" '(5 7)
      (let ((seen '()))
        (for-each (lambda (x y) (set! seen (cons (+ x y) seen)))
                  '(1 2 3)
                  '(4 5))
        (reverse seen)))
(test "for-each handles large lists" 20000
      (let ((count 0))
        (for-each (lambda (x) (set! count (+ count 1)))
                  (iota 20000))
        count))

;;; ============================================================================
;;; String ports
;;; ============================================================================

(section "String Ports")

(test "open-output-string" #t (output-port? (open-output-string)))
(test "get-output-string" "hello"
      (let ((p (open-output-string)))
        (display "hello" p)
        (get-output-string p)))
(test "get-output-string returns immutable string" #t
      (let ((p (open-output-string)))
        (display "a" p)
        (let ((s (get-output-string p)))
          (guard (e (#t #t))
            (string-set! s 0 #\x)
            #f))))
(test "open-input-string" #t (input-port? (open-input-string "test")))
(test "open-input-string range" "cd"
      (let ((p (open-input-string "abcdef" 2 4)))
        (list->string (list (read-char p) (read-char p)))))
(test "open-input-string Unicode range" #\x
      (read-char (open-input-string "λx" 1 2)))
(test "open-input-string observes source mutation" "zbc"
      (let ((source (string-copy "abc")))
        (let ((p (open-input-string source)))
          (string-set! source 0 #\z)
          (read-line p))))
(test "open-input-string retains source through allocation" "zbc"
      (let ((source (string-copy "abc")))
        (let ((p (open-input-string source)))
          (let loop ((n 20000))
            (if (= n 0)
                #f
                (begin
                  (cons n '())
                  (loop (- n 1)))))
          (string-set! source 0 #\z)
          (read-line p))))
(test "open-input-string tracks character position after re-encoding" #\b
      (let ((source (string-copy "ab")))
        (let ((p (open-input-string source)))
          (read-char p)
          (string-set! source 0 #\λ)
          (read-char p))))
(test "open-input-string tracks read-line position after re-encoding" #\d
      (let ((source (string-copy "abc\ndef")))
        (let ((p (open-input-string source)))
          (read-line p)
          (string-set! source 0 #\λ)
          (read-char p))))
(test "open-input-string validates range" #t
      (guard (e (#t #t))
        (open-input-string "abc" 3 2)
        #f))
(test "read-char from string" #\h
      (let ((p (open-input-string "hello")))
        (read-char p)))
(test "read-char-no-hang from string" #\h
      (read-char-no-hang (open-input-string "hello")))
(test "read-char-no-hang string eof" #t
      (eof-object? (read-char-no-hang (open-input-string ""))))
(test "read-char-no-hang special-file eof" #t
      (let ((port (open-input-file "/dev/null")))
        (eof-object? (read-char-no-hang port))))
(test "u8-ready? special-file eof" '(#t #t)
      (let ((port (open-binary-input-file "/dev/null")))
        (list (u8-ready? port)
              (eof-object? (read-u8 port)))))
(test "read/write-u8 binary file preserves bytes" '(195 169)
      (let ((path "/tmp/vesper-u8-roundtrip.bin"))
        (let ((out (open-binary-output-file path)))
          (write-u8 195 out)
          (write-u8 169 out)
          (close-output-port out))
        (let ((in (open-binary-input-file path)))
          (let ((result (list (read-u8 in) (read-u8 in))))
            (close-input-port in)
            (delete-file path)
            result))))
(test "call-with-binary-output-file writes and closes" '(#u8(0 255) #f)
      (let ((path "/tmp/vesper-call-with-binary-output-test.bin")
            (port #f))
        (call-with-binary-output-file
          path
          (lambda (p)
            (set! port p)
            (write-bytevector #u8(0 255) p)))
        (let ((in (open-binary-input-file path)))
          (let ((bytes (read-bytevector 2 in)))
            (close-input-port in)
            (delete-file path)
            (list bytes (output-port-open? port))))))
(test "bytevector file I/O rejects textual ports" '(#t #t #t)
      (list
       (guard (e (#t #t))
         (call-with-input-file "/dev/null"
           (lambda (port) (read-bytevector 0 port)))
         #f)
       (guard (e (#t #t))
         (call-with-output-file "/dev/null"
           (lambda (port) (write-bytevector #u8() port)))
         #f)
       (guard (e (#t #t))
         (call-with-input-file "/dev/null"
           (lambda (port)
             (read-bytevector! (bytevector) port)))
         #f)))
(test "textual I/O rejects binary file ports" '(#t #t #t #t #t)
      (list
       (guard (e (#t #t))
         (let ((port (open-binary-output-file "/dev/null")))
           (guard (e (begin (close-output-port port) #t))
             (write-char #\A port)
             (close-output-port port)
             #f))
         #f)
       (guard (e (#t #t))
         (let ((port (open-binary-output-file "/dev/null")))
           (guard (e (begin (close-output-port port) #t))
             (write-string "A" port)
             (close-output-port port)
             #f))
         #f)
       (guard (e (#t #t))
         (let ((port (open-binary-output-file "/dev/null")))
           (guard (e (begin (close-output-port port) #t))
             (display "A" port)
             (close-output-port port)
             #f))
         #f)
       (guard (e (#t #t))
         (let ((port (open-binary-output-file "/dev/null")))
           (guard (e (begin (close-output-port port) #t))
             (newline port)
             (close-output-port port)
             #f))
         #f)
       (guard (e (#t #t))
         (let ((port (open-binary-input-file "/dev/null")))
           (guard (e (begin (close-input-port port) #t))
             (read-char port)
             (close-input-port port)
             #f))
         #f)))
(test "unread-char string port" '(#\h #\h #\h)
      (call-with-input-string "hello"
        (lambda (port)
          (let ((first (read-char port)))
            (list first
                  (unread-char first port)
                  (read-char port))))))
(test "unread-char unicode string port" '(#t #t)
      (call-with-input-string "λx"
        (lambda (port)
          (let ((char (read-char port)))
            (list (char=? char (integer->char 955))
                  (begin (unread-char char port)
                         (char=? (read-char port) char)))))))
(test "unread-char validates most recent character" #t
      (guard (e (#t #t))
        (call-with-input-string "ab"
          (lambda (port)
            (read-char port)
            (unread-char #\b port)))
        #f))
(test "call-with-output-string" "test"
      (call-with-output-string (lambda (p) (display "test" p))))
(test "call-with-input-string" #\a
      (call-with-input-string "abc" (lambda (p) (read-char p))))
(test "with-output-to-string" "test"
      (with-output-to-string (display "test")))
(test "with-input-from-string" #\a
      (with-input-from-string "abc" (read-char)))
(test "with-output-to-string restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string))
            (escape #f))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (set! escape k)
            (with-output-to-string (escape #t))))
        (display "after")
        (set-current-output-port! old)
        (get-output-string p)))
(test "with-input-from-string restores on continuation escape" #\z
      (let ((old (current-input-port))
            (p (open-input-string "z"))
            (escape #f))
        (set-current-input-port! p)
        (let ((result
               (begin
                 (call/cc
                   (lambda (k)
                     (set! escape k)
                     (with-input-from-string "abc" (escape #t))))
                 (read-char))))
          (set-current-input-port! old)
          result)))
(test "string-port?" #t (string-port? (open-output-string)))
(test "string-port? file" #f (string-port? (current-output-port)))

;;; ============================================================================
;;; File Port Dynamic Extents
;;; ============================================================================

(section "File Port Dynamic Extents")

(test "call-with-output-file preserves port on continuation escape" #\x
      (let ((escaped-port #f))
        (call/cc
          (lambda (k)
            (call-with-output-file
              "/tmp/vesper-call-with-output-escape-test.txt"
              (lambda (p)
                (set! escaped-port p)
                (display "x" p)
                (flush-output-port p)
                (k #t)))))
        (let ((result
               (call-with-input-file
                 "/tmp/vesper-call-with-output-escape-test.txt"
                 (lambda (p) (read-char p)))))
          (close-output-port escaped-port)
          result)))

(test "with-output-to-file restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string))
            (escaped-port #f))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (with-output-to-file
              "/tmp/vesper-with-output-escape-test.txt"
              (lambda ()
                (set! escaped-port (current-output-port))
                (k #t)))))
        (display "after")
        (set-current-output-port! old)
        (close-output-port escaped-port)
        (get-output-string p)))

;;; ============================================================================
;;; Case and Do
;;; ============================================================================

(section "Case and Do")

(test "case match" 'composite
      (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))
(test "case else" 'other
      (case 10 ((1 2) 'small) ((3 4) 'medium) (else 'other)))
(test "case evaluates key once" 1
      (let ((count 0))
        (case (begin (set! count (+ count 1)) 'c)
          ((a) 'a)
          ((b) 'b)
          ((c) count)
          (else 'other))))
(test "do loop" 15
      (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) (+ sum i))))

;;; ============================================================================
;;; Delay/Force
;;; ============================================================================

(section "Delay and Force")

(test "delay/force" 10 (force (delay (+ 5 5))))
(test "force memoizes" #t
      (let* ((count 0)
             (p (delay (begin (set! count (+ count 1)) count))))
        (force p)
        (force p)
        (= count 1)))
(test "promise? delay" #t (promise? (delay 1)))
(test "promise? non-promise" #f (promise? (lambda () 1)))
(test "make-promise" 42 (force (make-promise 42)))
(test "make-promise preserves promise" #t
      (let ((p (delay 3)))
        (eq? p (make-promise p))))
(test "delay-force" 9 (force (delay-force (delay (+ 4 5)))))
(test "eof-object" #t (eof-object? (eof-object)))

;;; ============================================================================
;;; Bignums
;;; ============================================================================

(section "Bignums")

(test "bignum add" 20000000000000000000 (+ 10000000000000000000 10000000000000000000))
(test "bignum mult" 100000000000000000000000000000000000000 (* 10000000000000000000 10000000000000000000))
(test "factorial 50" 30414093201713378043612608166064768844377641568960512000000000000
      (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 50)))
(test "random-integer accepts bignum bound" #t
      (let* ((bound (expt 2 70))
             (value (random-integer bound)))
        (and (exact-integer? value)
             (>= value 0)
             (< value bound))))

;;; ============================================================================
;;; Multiple values
;;; ============================================================================

(section "Multiple Values")

(test "values/call-with-values" 3
      (call-with-values (lambda () (values 1 2)) +))
(test "call/cc continuation accepts multiple values" '(1 2)
      (call-with-values
        (lambda () (call/cc (lambda (k) (k 1 2))))
        list))
(test "call/cc continuation accepts zero values" '()
      (call-with-values
        (lambda () (call/cc (lambda (k) (k))))
        list))
(test "dynamic-wind continuation forwards multiple values" '((a b) (before after))
      (let ((log '()))
        (list
          (call-with-values
            (lambda ()
              (dynamic-wind
                (lambda () (set! log (cons 'before log)))
                (lambda ()
                  (call/cc
                    (lambda (k)
                      (k 'a 'b))))
                (lambda () (set! log (cons 'after log)))))
            list)
          (reverse log))))

(test "dynamic-wind rejects circular wind stacks"
      #t
      (let ((cycle (cons (cons (lambda () #f) (lambda () #f)) '())))
        (set-cdr! cycle cycle)
        (guard (e (#t #t))
          (do-wind cycle '())
          #f)))

(define-values (defined-a defined-b) (values 7 8))
(test "define-values" 15 (+ defined-a defined-b))
(test "define-values enforces fixed-formal value count" '(#t #t #t)
      (list
       (guard (e (#t #t))
         (let () (define-values (a) (values 1 2)) a)
         #f)
       (guard (e (#t #t))
         (let () (define-values () (values 1)) #t)
         #f)
       (guard (e (#t #t))
         (let () (define-values (a b) (values 1)) a)
         #f)))
(test "let-values parallel"
      3
      (let ((x 1))
        (let-values (((x y) (values 2 x)))
          (+ x y))))
(test "let-values supports multiple parallel clauses"
      '(3 (1 10))
      (list
       (let-values (((a) (values 1))
                    ((b) (values 2)))
         (+ a b))
       (let ((x 10))
         (let-values (((x) (values 1))
                      ((y) (values x)))
           (list x y)))))
(test "let*-values sequential"
      5
      (let*-values (((x y) (values 2 1))
                    ((z) (+ x y)))
        (+ z x)))
(define arity-proc
  (case-lambda
    (() 'zero)
    ((x) x)
    ((x y) (+ x y))
    ((a b c d e) (+ a b c d e))))
(test "case-lambda zero" 'zero (arity-proc))
(test "case-lambda one" 4 (arity-proc 4))
(test "case-lambda two" 9 (arity-proc 4 5))
(test "case-lambda five" 15 (arity-proc 1 2 3 4 5))
(define rest-arity-proc
  (case-lambda
    ((x y . rest) rest)
    (args args)))
(test "case-lambda dotted rest" '(3 4) (rest-arity-proc 1 2 3 4))
(test "case-lambda rest formals" '(1) (rest-arity-proc 1))
(test "dynamic-wind preserves multiple values" '((1 2) (before after))
      (let ((events '()))
        (list
         (call-with-values
           (lambda ()
             (dynamic-wind
               (lambda () (set! events (cons 'before events)))
               (lambda () (values 1 2))
               (lambda () (set! events (cons 'after events)))))
           list)
         (reverse events))))

;;; ============================================================================
;;; List Utilities
;;; ============================================================================

(section "List Utilities")

(test "filter" '(2 4 6) (filter even? '(1 2 3 4 5 6)))
(test "remove" '(1 3 5) (remove even? '(1 2 3 4 5 6)))
(test "list-deletor" '(1 3 5) ((list-deletor even?) '(1 2 3 4 5 6)))
(test "find" 4 (find even? '(1 3 4 5 6)))
(test "find not found" #f (find even? '(1 3 5 7)))
(test "any true" #t (any even? '(1 3 4 5)))
(test "any false" #f (any even? '(1 3 5 7)))
(test "every true" #t (every odd? '(1 3 5 7)))
(test "every false" #f (every odd? '(1 2 3 4)))
(test "count" 3 (count even? '(1 2 3 4 5 6)))
(test "count multiple lists" 3
      (count < '(1 2 4 8) '(2 4 6 8 10 12)))
(test "fold" 15 (fold + 0 '(1 2 3 4 5)))
(test "fold-right" '(1 2 3 4 5) (fold-right cons '() '(1 2 3 4 5)))
(test "fold multiple lists" '(c 3 b 2 a 1)
      (fold cons* '() '(a b c) '(1 2 3 4 5)))
(test "fold-right multiple lists" '(a 1 b 2 c 3)
      (fold-right cons* '() '(a b c) '(1 2 3 4 5)))
(test "pair-fold multiple lists" '(c 3 b 2 a 1)
      (pair-fold (lambda (xs ys acc)
                   (cons* (car xs) (car ys) acc))
                 '() '(a b c) '(1 2 3 4 5)))
(test "pair-fold-right multiple lists" '(a 1 b 2 c 3)
      (pair-fold-right (lambda (xs ys acc)
                         (cons* (car xs) (car ys) acc))
                       '() '(a b c) '(1 2 3 4 5)))
(test "pair-for-each multiple lists and shortest termination"
      '((a 1) (b 2) (c 3))
      (let ((seen '()))
        (pair-for-each (lambda (xs ys)
                         (set! seen (cons (list (car xs) (car ys)) seen)))
                       '(a b c) '(1 2 3 4))
        (reverse seen)))
(test "pair-for-each finite plus circular list"
      '((a x) (b y) (c x))
      (let ((seen '())
            (cycle (circular-list 'x 'y)))
        (pair-for-each (lambda (xs ys)
                         (set! seen (cons (list (car xs) (car ys)) seen)))
                       '(a b c) cycle)
        (reverse seen)))
(test "pair-for-each snapshots successors before mutation"
      '(a b c)
      (let ((lst (list 'a 'b 'c))
            (seen '()))
        (pair-for-each (lambda (pair)
                         (set! seen (cons (car pair) seen))
                         (set-cdr! pair '()))
                       lst)
        (reverse seen)))
(test "filter-map multiple lists" '(3 18)
      (filter-map (lambda (a b) (if (< a b) (+ a b) #f))
                  '(1 4 8) '(2 3 10)))
(test "append-map!" '(1 2 3 4)
      (append-map! (lambda (x) (list x (+ x 1))) '(1 3)))
(test "append! replaces dotted prefix tail" '(1 2 4)
      (append! '(1 2 . 3) '(4)))
(test "map! rejects shorter companion before mutation"
      'ok
      (let ((lst (list 1 2 3)))
        (guard (e (#t (if (equal? lst '(1 2 3)) 'ok 'mutated)))
          (map! + lst '(10 20))
          'no-error)))
(test "map! skips unused companion on empty input" 'ok
      (begin (map! #f '() 1) 'ok))
(test "map! accepts a long circular companion"
      '(11 22 13)
      (let ((lst (list 1 2 3)))
        (map! + lst (circular-list 10 20))
        lst))
(test "delete-duplicates preserves one false value" '(#f)
      (delete-duplicates '(#f #f)))
(test "empty list searches skip unused comparators"
      '(#f #f () () () () () ())
      (list (member 1 '() #f)
            (assoc 1 '() #f)
            (delete 1 '() #f)
            (delete-duplicates '() #f)
            (delete! 1 '() #f)
            (delete-duplicates! '() #f)
            (alist-delete 1 '() #f)
            (alist-delete! 1 '() #f)))
(test "reduce-right uses final element as base" '(1 . 2)
      (reduce-right cons 'end '(1 2)))
(test "reduce-right singleton skips identity" 4
      (reduce-right + 99 '(4)))
(test "alist->hash-table last duplicate wins" 2
      (hash-table-ref (alist->hash-table '((a . 1) (a . 2))) 'a))
(test "take" '(1 2 3) (take '(1 2 3 4 5) 3))
(test "drop" '(4 5) (drop '(1 2 3 4 5) 3))
(test "take/drop dotted list" '((1 2) (3 . end))
      (let ((lst (cons 1 (cons 2 (cons 3 'end)))))
        (list (take lst 2) (drop lst 2))))
(test "take/drop reject counts beyond finite list" '(#t #t)
      (list
       (guard (e (#t #t)) (take '(1 2 3) 4) #f)
       (guard (e (#t #t)) (drop '(1 2 3) 4) #f)))
(test "list-copy preserves dotted tail" '(1 2 3 . end)
      (list-copy (cons 1 (cons 2 (cons 3 'end)))))
(test "count-pairs handles proper, dotted, circular, prefixed, and atomic objects"
      '(3 3 3 4 0)
      (let ((cycle (circular-list 'a 'b 'c)))
        (list (count-pairs '(1 2 3))
              (count-pairs (cons 1 (cons 2 (cons 3 'end))))
              (count-pairs cycle)
              (count-pairs (cons 'prefix cycle))
              (count-pairs 42))))
(test "finite-list consumers handle atoms" '(42 #t #t)
      (list
       (list-copy 42)
       (guard (e (#t #t)) (take-right 42 0) #f)
       (guard (e (#t #t)) (drop-right 42 0) #f)))
(test "take/drop circular list" '((a b a b) (b a b a))
      (let ((lst (circular-list 'a 'b)))
        (list (take lst 4) (take (drop lst 3) 4))))
(test "take-right rejects count beyond length" #t
      (guard (e (#t #t)) (take-right '(1 2 3) 5) #f))
(test "drop-right rejects count beyond length" #t
      (guard (e (#t #t)) (drop-right '(1 2 3) 5) #f))
(test "split-at rejects count beyond length" #t
      (guard (e (#t #t))
        (call-with-values
          (lambda () (split-at '(1 2 3) 5))
          list)
        #f))
(test "split-at! rejects count beyond length" #t
      (guard (e (#t #t))
        (let ((lst (list 1 2 3)))
          (call-with-values
            (lambda () (split-at! lst 5))
            list))
        #f))
(test "take! rejects count beyond length" #t
      (guard (e (#t #t))
        (let ((lst (list 1 2 3)))
          (take! lst 5))
        #f))
(test "cut multiple slots" '(1 2 3 4 5)
      ((cut list 1 <> 3 <> 5) 2 4))
(test "cut rest slot" '(1 2 3 4 5)
      ((cut list 1 <> 3 <...>) 2 4 5))
(test "cute multiple slots" '(1 2 3 4 5)
      ((cute list 1 <> 3 <> 5) 2 4))
(test "partition" '((2 4 6) (1 3 5))
      (call-with-values
        (lambda () (partition even? '(1 2 3 4 5 6)))
        list))
(test "zip" '((1 a) (2 b) (3 c)) (zip '(1 2 3) '(a b c)))
(test "zip one list" '((1) (2) (3)) (zip '(1 2 3)))
(test "zip multiple lists" '((1 a x) (2 b y))
      (zip '(1 2 3) '(a b) '(x y z)))
(test "circular-list? and length+" '(#t #f 3 #f)
      (let ((c (circular-list 'a 'b 'c)))
        (list (circular-list? c)
              (circular-list? '(a b c))
              (length+ '(a b c))
              (length+ c))))
(test "make-circular-list" '(#t #t)
      (let ((c (make-circular-list 2 'x)))
        (list (circular-list? c)
              (eq? (cddr c) c))))
(test "flatten" '(1 2 3 4 5) (flatten '(1 (2 (3 4) 5))))
(test "last" 5 (last '(1 2 3 4 5)))
(test "last rejects empty, dotted, and circular lists" '(#t #t #t)
      (list
       (guard (e (#t #t)) (last '()) #f)
       (guard (e (#t #t)) (last (cons 1 2)) #f)
       (guard (e (#t #t)) (last (circular-list 1 2)) #f)))
(test "iota" '(0 1 2 3 4) (iota 5))
(test "iota validates count and arity" '(#t #t)
      (list
       (guard (e (#t #t)) (iota -1) #f)
       (guard (e (#t #t)) (iota 3 0 1 2) #f)))
(test "list counts require exact integers" '(#t #t)
      (list
       (guard (e (#t #t)) (list-ref '(a) 0.0) #f)
       (guard (e (#t #t)) (make-list 1.0) #f)))
(test "list optional arguments validate arity" '(#t #t #t #t)
      (list
       (guard (e (#t #t)) (member 1 '(1) equal? eq?) #f)
       (guard (e (#t #t)) (assoc 'a '((a . 1)) equal? eq?) #f)
       (guard (e (#t #t)) (delete 1 '(1) equal? eq?) #f)
       (guard (e (#t #t)) (make-list 1 #f #f) #f)))
(test "range" '(2 3 4) (range 2 5))
(test "range supports negative endpoints" '(-2 -1 0 1)
      (range -2 2))
(test "range validates endpoints" '(#t #t)
      (list
       (guard (e (#t #t)) (range 1.5 3) #f)
       (guard (e (#t #t)) (range 1 'three) #f)))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(newline)
(display "========================================")
(newline)
(display "Tests: ")
(display test-count)
(display ", Passed: ")
(display pass-count)
(display ", Failed: ")
(display fail-count)
(newline)

;;; ============================================================================
;;; Continuations (call/cc)
;;; ============================================================================

(section "Continuations")

; Simple early exit
(test "call/cc early exit"
      10
      (+ 1 (call/cc (lambda (k) (+ 2 (k 9))))))

; Continuation captures the computation context
(test "call/cc captures context"
      15
      (+ 5 (call/cc (lambda (k) (k 10)))))

; Continuation ignores code after k is called
(test "call/cc aborts"
      10
      (call/cc (lambda (k)
                 (k 10)
                 (error "should not reach here"))))

; Escape from loop
(test "call/cc escape loop"
      5
      (call/cc
        (lambda (exit)
          (let loop ((n 0))
            (if (= n 5)
                (exit n)
                (loop (+ n 1)))))))

; Exception-like behavior
(define (safe-div a b)
  (call/cc
    (lambda (return)
      (if (= b 0)
          (return 'error)
          (/ a b)))))

(test "call/cc exception div ok" 5 (safe-div 10 2))
(test "call/cc exception div zero" 'error (safe-div 10 0))

; Simple amb-style backtracking (limited)
(define *fail* #f)

(define (choose lst)
  (if (null? lst)
      (*fail* 'fail)
      (call/cc
        (lambda (cc)
          (let ((old-fail *fail*))
            (set! *fail*
                  (lambda (x)
                    (set! *fail* old-fail)
                    (cc (choose (cdr lst)))))
            (car lst))))))

(define (require pred)
  (if (not pred) (*fail* 'fail)))

; Find a pair where a < b and a + b = 7
(define amb-result
  (call/cc
    (lambda (top-exit)
      (set! *fail* (lambda (x) (top-exit 'no-solution)))
      (let ((a (choose '(1 2 3 4 5)))
            (b (choose '(1 2 3 4 5))))
        (require (< a b))
        (require (= (+ a b) 7))
        (list a b)))))

; Verify result satisfies constraints
(test "amb-style choose valid"
      #t
      (and (pair? amb-result)
           (< (car amb-result) (cadr amb-result))
           (= 7 (+ (car amb-result) (cadr amb-result)))))

; Generator-style producer/consumer
(define (make-counter max)
  (let ((return #f)
        (count 0))
    (lambda ()
      (call/cc
        (lambda (k)
          (set! return k)
          (let loop ()
            (if (> count max)
                (return 'done)
                (call/cc
                  (lambda (next)
                    (set! return next)
                    (let ((c count))
                      (set! count (+ count 1))
                      (k c)))))
            (loop)))))))

(define gen (make-counter 3))
(test "generator 0" 0 (gen))
(test "generator 1" 1 (gen))
(test "generator 2" 2 (gen))
(test "generator 3" 3 (gen))
(test "generator done" 'done (gen))

;;; ============================================================================
;;; Continuation Stress Tests (GC + multi-shot continuations)
;;; ============================================================================

(section "Continuation Stress")

; Full amb implementation with heavy multi-shot reuse
(define fail-cont #f)

(define (fail)
  (if fail-cont
      (fail-cont 'fail)
      #f))

(define (amb . choices)
  (call/cc
   (lambda (return-choice)
     (let try ((remaining choices))
       (if (null? remaining)
           (fail)
           (call/cc
            (lambda (save-fail)
              (set! fail-cont (lambda (_) (save-fail 'next)))
              (return-choice (car remaining)))))))))

; Stress test: 100 amb iterations
(define amb-count 0)
(call/cc
 (lambda (escape)
   (set! fail-cont (lambda (_) (escape 'done)))
   (let loop ()
     (let ((x (amb 1 2 3 4 5)))
       (set! amb-count (+ amb-count 1))
       (if (>= amb-count 100)
           (escape 'done)
           (fail))))))

(test "amb stress 100 iterations" 100 amb-count)

; Generator implementation with multi-shot continuations
(define (make-generator proc)
  (let ((resume-point #f)
        (caller-cont #f))
    (lambda ()
      (call/cc
       (lambda (caller)
         (set! caller-cont caller)
         (if resume-point
             (resume-point 'continue)
             (begin
               (proc (lambda (value)
                       (call/cc
                        (lambda (k)
                          (set! resume-point k)
                          (caller-cont value)))))
               (caller-cont #f))))))))

(define (range-gen start end)
  (make-generator
   (lambda (yield)
     (let loop ((i start))
       (if (<= i end)
           (begin
             (yield i)
             (loop (+ i 1))))))))

; Stress test: generator with 50 iterations
(define gen-stress (range-gen 1 50))
(define gen-sum 0)
(let loop ()
  (let ((val (gen-stress)))
    (if val
        (begin
          (set! gen-sum (+ gen-sum val))
          (loop)))))

(test "generator stress sum 1-50" 1275 gen-sum)

;;; ============================================================================
;;; Edge Cases and New Predicates
;;; ============================================================================

(section "Numeric Predicates (R5RS)")

; finite?, infinite?, nan? predicates
(test "finite? integer" #t (finite? 42))
(test "finite? float" #t (finite? 3.14159))
(test "finite? rational" #t (finite? 22/7))
(test "finite? bignum" #t (finite? 99999999999999999999))
(test "finite? infinity" #f (finite? (exp 1000)))
(test "infinite? integer" #f (infinite? 0))
(test "infinite? overflow" #t (infinite? (exp 1000)))
(test "nan? normal" #f (nan? 42))
(test "log -1 is complex" #t (complex? (log -1)))
(test "log -1 real-part" 0.0 (real-part (log -1)))

; complex number predicates
(test "finite? complex" #t (finite? (make-rectangular 3 4)))
(test "infinite? complex" #t (infinite? (make-rectangular (exp 1000) 0)))
(test "nan? complex" #f (nan? (make-rectangular 3 4)))

(section "String Edge Cases")

; Empty strings
(test "empty string length" 0 (string-length ""))
(test "empty string->list" '() (string->list ""))
(test "empty list->string" "" (list->string '()))
(test "empty string append" "hello" (string-append "" "hello" ""))
(test "empty substring" "" (substring "hello" 2 2))

; Long strings
(define long-str (make-string 1000 #\x))
(test "long string length" 1000 (string-length long-str))
(test "long string ref" #\x (string-ref long-str 999))

(section "Vector Edge Cases")

; Empty vectors
(test "empty vector length" 0 (vector-length '#()))
(test "empty vector->list" '() (vector->list '#()))
(test "empty list->vector" '#() (list->vector '()))

; Vector modification
(define v (make-vector 3 0))
(vector-set! v 0 'a)
(vector-set! v 2 'c)
(test "vector-set!" '#(a 0 c) v)

(section "Complex Numbers")

; Complex arithmetic
(test "complex magnitude" 5.0 (magnitude (make-rectangular 3 4)))
(test "complex real-part" 3 (real-part (make-rectangular 3 4)))
(test "complex imag-part" 4 (imag-part (make-rectangular 3 4)))
(test "real imag-part" 0 (imag-part 5))
(test "real real-part" 5 (real-part 5))

; Complex predicates
(test "complex? true" #t (complex? (make-rectangular 1 2)))
(test "complex? real" #t (complex? 5))  ; all reals are complex
(test "rational? int" #t (rational? 5))
(test "rational? frac" #t (rational? 3/4))

(section "Rational Numbers")

; numerator/denominator
(test "numerator int" 5 (numerator 5))
(test "denominator int" 1 (denominator 5))
(test "numerator frac" 3 (numerator 3/4))
(test "denominator frac" 4 (denominator 3/4))

(section "Bignum Rationals")

; Reading bignum rationals
(define big-rat 99999999999999999999/7)
(test "bignum rational read" 99999999999999999999/7 big-rat)
(test "bignum reduces to int" 33333333333333333333 99999999999999999999/3)
(test "bignum/bignum reduces" 9/8 99999999999999999999/88888888888888888888)
(test "bignum rational numerator" 1 (numerator 1/99999999999999999999))
(test "bignum rational denom" 99999999999999999999 (denominator 1/99999999999999999999))

; Arithmetic with bignum rationals
(test "bignum + rational" 199999999999999999999/2 (+ 99999999999999999999 1/2))
(test "bignum * rational" 99999999999999999999/2 (* 99999999999999999999 1/2))

(section "Exactness")

; exact/inexact conversions
(test "exact? int" #t (exact? 42))
(test "inexact? float" #t (inexact? 3.14))
(test "exact->inexact" 5.0 (exact->inexact 5))
(test "inexact->exact" 5 (inexact->exact 5.0))

;;; ============================================================================
;;; R7RS/MIT Scheme Compatibility Additions
;;; ============================================================================

(section "Compatibility Additions")

; Parameters
(define sample-parameter (make-parameter 10))
(test "parameter initial value" 10 (sample-parameter))
(sample-parameter 12)
(test "parameter set value" 12 (sample-parameter))
(test "parameterize dynamic value"
      '(99 12)
      (list (parameterize ((sample-parameter 99)) (sample-parameter))
            (sample-parameter)))
(test "make-parameter rejects excess converters" #t
      (guard (e (#t #t))
        (make-parameter 1 (lambda (x) x) (lambda (x) x))
        #f))

; cond-expand and feature inquiry
(test "features contains srfi-1" #t (if (memq 'srfi-1 (features)) #t #f))
(test "cond-expand selects feature"
      'has-bytevectors
      (cond-expand
        (bytevectors 'has-bytevectors)
        (else 'missing)))
(test "cond-expand supports and/not"
      'ok
      (cond-expand
        ((and vesper (not imaginary-feature)) 'ok)
        (else 'bad)))

; Hash tables
(define ht (make-hash-table))
(test "hash-table? true" #t (hash-table? ht))
(test "hash-table? false" #f (hash-table? '()))
(hash-table-set! ht "answer" 42)
(hash-table-set! ht '(a b) 'list-key)
(test "hash-table-ref string key" 42 (hash-table-ref ht "answer"))
(test "hash-table-ref equal list key" 'list-key (hash-table-ref ht '(a b)))

;; scheme_hash had no BT_INEXACT case, so flonums hashed by cell index while
;; hash_key_equal compares them via deep_equal (by CELL_ID, the double's bit
;; pattern). Two equal? flonums therefore landed in different buckets and an
;; inexact key could never be found again - directly, or nested inside a list.
;; Verified against MIT Scheme. Kept in its own table: `ht` has a pinned size
;; assertion further down.
(define inexact-ht (make-hash-table))
(hash-table-set! inexact-ht 1.5 'flonum-key)
(hash-table-set! inexact-ht (list 2.5 'x) 'nested-flonum-key)
(test "hash-table-ref inexact key" 'flonum-key (hash-table-ref inexact-ht 1.5))
(test "hash-table-ref list containing inexact" 'nested-flonum-key
      (hash-table-ref inexact-ht (list 2.5 'x)))
(test "hash-table-ref inexact key computed at runtime" 'flonum-key
      (hash-table-ref inexact-ht (/ 3.0 2.0)))
(test "hash-table-ref distinguishes inexact from exact" 'missing
      (hash-table-ref inexact-ht 2 (lambda () 'missing)))
(test "hash-table-ref missing thunk" 'missing
      (hash-table-ref ht 'none (lambda () 'missing)))
(test "hash-table-ref invokes default thunk" 'thunk
      (hash-table-ref ht 'none (lambda () 'thunk)))
(test "hash-table-ref does not invoke default on hit" 42
      (hash-table-ref ht "answer" (lambda () 'wrong)))
(test "hash-table-ref requires default thunk" #t
      (guard (e (#t #t))
        (hash-table-ref ht 'none 'not-a-thunk)
        #f))
(test "guard catches malformed application" #t
      (guard (e (#t #t))
        ((quote not-a-procedure) . extra)
        #f))
(test "guard catches set! of unbound variable" #t
      (guard (e (#t #t))
        (set! definitely-unbound-variable 1)
        #f))
(test "hash-table-exists?" #t (hash-table-exists? ht "answer"))
(test "hash-table-size" 2 (hash-table-size ht))
(hash-table-delete! ht "answer")
(test "hash-table-delete!" #f (hash-table-exists? ht "answer"))
(define resize-ht (make-hash-table 1))
(let loop ((i 0))
  (if (< i 100)
      (begin
        (hash-table-set! resize-ht i (* i i))
        (loop (+ i 1)))))
(test "hash-table resizes" 9801 (hash-table-ref resize-ht 99))
(test "hash-table size after resize" 100 (hash-table-size resize-ht))
(define equal-ht (make-equal-hash-table))
(hash-table-set! equal-ht '(x y) 17)
(test "make-equal-hash-table uses equal?" 17 (hash-table-ref equal-ht '(x y)))
(define eq-key (list 'x 'y))
(define eq-ht (make-strong-eq-hash-table))
(hash-table-set! eq-ht eq-key 23)
(test "make-strong-eq-hash-table same object" 23 (hash-table-ref eq-ht eq-key))
(test "make-strong-eq-hash-table not equal list" 'missing
      (hash-table-ref eq-ht '(x y) (lambda () 'missing)))
(test "hash-table-equivalence-function eq?" #f
      ((hash-table-equivalence-function eq-ht)
       eq-key
       (list 'x 'y)))
(define eq-copy (hash-table-copy eq-ht))
(test "hash-table-copy preserves eq?" 'missing
      (hash-table-ref eq-copy (list 'x 'y) (lambda () 'missing)))
(define eqv-ht (make-strong-eqv-hash-table))
(hash-table-set! eqv-ht 1000 'numeric)
(hash-table-set! eqv-ht "key" 'string-object)
(test "make-strong-eqv-hash-table numeric" 'numeric (hash-table-ref eqv-ht 1000))
(test "make-strong-eqv-hash-table not string equal" 'missing
      (hash-table-ref eqv-ht (string-append "k" "ey") (lambda () 'missing)))
(test "hash-table-equivalence-function eqv?" #t
      ((hash-table-equivalence-function eqv-ht) 1000 1000))
(test "hash-table-equivalence-function equal?" #t
      ((hash-table-equivalence-function equal-ht) '(x y) '(x y)))
(define equal-hash (hash-table-hash-function equal-ht))
(test "hash-table-hash-function equal keys" #t
      (= (equal-hash '(x y)) (equal-hash '(x y))))
(test "hash-table-hash-function bound" #t
      (let ((h (equal-hash '(x y) 17)))
        (and (>= h 0) (< h 17))))
(test "hash equal keys" #t
      (= (hash '(x y)) (hash '(x y))))
(test "hash bound" #t
      (let ((h (hash '(x y) 19)))
        (and (>= h 0) (< h 19))))
(test "hash-by-identity bound" #t
      (let ((h (hash-by-identity (list 'x 'y) 23)))
        (and (>= h 0) (< h 23))))
(test "hash validates bounds" '(#t #t #t #t)
      (list
       (guard (e (#t #t)) (hash 'x 0) #f)
       (guard (e (#t #t)) (hash 'x -1) #f)
       (guard (e (#t #t)) (hash 'x 1.5) #f)
       (guard (e (#t #t)) (hash 'x 10 20) #f)))
(define eqv-copy (hash-table-copy eqv-ht))
(test "hash-table-copy preserves eqv?" 'numeric
      (hash-table-ref eqv-copy 1000))
(test "hash-table-copy eqv rejects equal string object" 'missing
      (hash-table-ref eqv-copy (string-append "k" "ey") (lambda () 'missing)))
(test "hash-table-keys" #t (if (member '(a b) (hash-table-keys ht)) #t #f))
(test "hash-table-values" #t (if (memq 'list-key (hash-table-values ht)) #t #f))
(test "hash-table->alist" #t (if (assoc '(a b) (hash-table->alist ht)) #t #f))
(define alist-ht (alist->hash-table '((first . 1) (second . 2) (first . 9))))
(test "alist->hash-table last duplicate wins" 9
      (hash-table-ref alist-ht 'first))
(test "alist->hash-table preserves values" 2
      (hash-table-ref alist-ht 'second))
(test "alist->hash-table validates optional arguments" #t
      (guard (e (#t #t))
        (alist->hash-table '((a . 1)) 64)
        #f))
(test "alist->hash-table validates entries" #t
      (guard (e (#t #t))
        (alist->hash-table '((a . 1) b))
        #f))
(hash-table-update! ht '(a b) (lambda (x) (list x 'updated)))
(test "hash-table-update!" '(list-key updated) (hash-table-ref ht '(a b)))
(test "hash-table-update! validates arity" #t
      (guard (e (#t #t))
        (hash-table-update! ht 'missing values (lambda () 1) (lambda () 2))
        #f))
(test "hash-table-ref/default" 'fallback
      (hash-table-ref/default ht 'absent 'fallback))
(hash-table-update!/default ht 'defaulted (lambda (x) (+ x 1)) 41)
(test "hash-table-update!/default" 42
      (hash-table-ref ht 'defaulted))
(define merge-target (make-hash-table))
(hash-table-set! merge-target 'old 1)
(define merge-source (make-hash-table))
(hash-table-set! merge-source 'old 2)
(hash-table-set! merge-source 'new 3)
(test "hash-table-merge! returns target" #t
      (eq? merge-target (hash-table-merge! merge-target merge-source)))
(test "hash-table-merge! overwrites" 2
      (hash-table-ref merge-target 'old))
(test "hash-table-merge! adds entries" 3
      (hash-table-ref merge-target 'new))
(define walk-sum 0)
(hash-table-walk resize-ht (lambda (k v) (set! walk-sum (+ walk-sum 1))))
(test "hash-table-walk" 100 walk-sum)
(test "hash-table-fold" 100
      (hash-table-fold resize-ht (lambda (k v acc) (+ acc 1)) 0))
(test "hash-table-map" #t
      (if (member 9801 (hash-table-map (lambda (k v) v) resize-ht)) #t #f))
(test "hash-table-map validates callback on empty table" #t
      (guard (e (#t #t))
        (hash-table-map #f (make-hash-table))
        #f))
(define resize-ht-copy (hash-table-copy resize-ht))
(test "hash-table-copy size" 100 (hash-table-size resize-ht-copy))
(test "hash-table-copy value" 9801 (hash-table-ref resize-ht-copy 99))
(hash-table-clear! resize-ht)
(test "hash-table-clear!" 0 (hash-table-size resize-ht))

; Sorting helpers
(test "list-sort" '(1 2 3 4) (list-sort < '(4 2 3 1)))
(test "sorting validates procedures on empty sequences" '(#t #t #t)
      (list (guard (e (#t #t)) (list-sort #f '()) #f)
            (guard (e (#t #t)) (list-sort < '() #f) #f)
            (guard (e (#t #t)) (sort! (list) #f) #f)))
(test "list-sort key" '((1 a) (2 b) (3 c))
      (list-sort < '((2 b) (1 a) (3 c)) car))
(test "merge-sort alias" '(1 2 3) (merge-sort '(3 1 2) <))
(test "quick-sort alias" '(1 2 3) (quick-sort '(3 1 2) <))
(test "stable-sort list" '((1 a) (1 c) (2 b))
      (stable-sort '((1 a) (2 b) (1 c))
                   (lambda (a b) (< (car a) (car b)))))
(test "stable-sort key" '((1 a) (1 c) (2 b))
      (stable-sort '((1 a) (2 b) (1 c)) < car))
(test "sort vector" '#(1 2 3) (sort '#(3 1 2) <))
(test "vector-sort key" '#((1 a) (2 b))
      (vector-sort < '#((2 b) (1 a)) car))
(define vector-sort-target '#(3 1 2))
(vector-sort! < vector-sort-target)
(test "vector-sort!" '#(1 2 3) vector-sort-target)
(define merge-sort-vector-target '#(3 1 2))
(merge-sort! merge-sort-vector-target <)
(test "merge-sort! alias" '#(1 2 3) merge-sort-vector-target)

;; merge was rewritten from (cons x (loop ...)) to an accumulate-and-unwind
;; loop so it runs in constant stack depth (see stress_tests.scm for the
;; 100000-deep case). Stability depends on taking from the right list only
;; when it is strictly less, so pin that precisely: with equal keys the
;; original relative order must survive, across more than two runs.
(test "merge preserves order of equal elements" '(x y a b c)
      (map cdr (merge (lambda (p q) (< (car p) (car q)))
                      '((0 . x) (1 . a) (1 . b))
                      '((0 . y) (1 . c)))))
(test "sort is stable across interleaved equal keys" '(x y a b c)
      (map cdr (sort '((1 . a) (0 . x) (1 . b) (0 . y) (1 . c)) < car)))
(test "sort handles duplicates" '(1 1 2 3 3) (sort '(3 1 3 1 2) <))
(test "sort of empty and singleton" '(() (1))
      (list (sort '() <) (sort '(1) <)))
(test "merge with an empty side" '((1 2 3) (1 2 3))
      (list (merge < '() '(1 2 3)) (merge < '(1 2 3) '())))
(test "sort leaves its input list unmodified" '(3 1 2)
      (let ((original (list 3 1 2)))
        (sort original <)
        original))
(test "merge" '(1 2 3 4 5 6) (merge < '(1 3 5) '(2 4 6)))

; I/O helpers and port predicates
(define out-str-port (open-output-string))
(write-string "abc" out-str-port)
(test "write-string" "abc" (get-output-string out-str-port))
(define out-str-port2 (open-output-string))
(write-string "abcdef" out-str-port2 2 5)
(test "write-string range" "cde" (get-output-string out-str-port2))
(define in-str-port (open-input-string "abcdef"))
(test "read-string" "abc" (read-string 3 in-str-port))
(define unicode-in-str-port (open-input-string "λx"))
(test "peek-char unicode" #\λ (peek-char unicode-in-str-port))
(test "read-char unicode" #\λ (read-char unicode-in-str-port))
(test "read-string unicode character count" "x"
      (read-string 1 unicode-in-str-port))
(test "read-string advances source-backed port once" #\c
      (let* ((source (string-copy "abcd"))
             (port (open-input-string source 1 3)))
        (read-string 1 port)
        (string-set! source 1 #\B)
        (read-char port)))
(test "read-line returns immutable string" #t
      (let ((s (read-line (open-input-string "a\n"))))
        (guard (e (#t #t))
          (string-set! s 0 #\x)
          #f)))
(define unicode-out-str-port (open-output-string))
(write-char #\λ unicode-out-str-port)
(test "write-char unicode" "λ" (get-output-string unicode-out-str-port))
(define unicode-range-out-str-port (open-output-string))
(write-string "λx" unicode-range-out-str-port 1 2)
(test "write-string unicode range" "x"
      (get-output-string unicode-range-out-str-port))
(test "input-port-open?" #t (input-port-open? in-str-port))
(close-input-port in-str-port)
(test "input-port-open? closed" #f (input-port-open? in-str-port))
(define crlf-port (open-input-string "alpha\r\nbeta\rgamma\n"))
(test "read-line preserves carriage return before lf"
      "alpha\r" (read-line crlf-port))
(test "read-line treats carriage return as ordinary text"
      "beta\rgamma" (read-line crlf-port))
(test "read-line lf after carriage return text" (eof-object)
      (read-line crlf-port))
(define peek-u8-port (open-input-string "AZ"))
(test "binary I/O rejects textual ports" '(#t #t #t #t)
      (list (guard (e (#t #t)) (write-u8 65 (open-output-string)) #f)
            (guard (e (#t #t)) (read-u8 peek-u8-port) #f)
            (guard (e (#t #t)) (peek-u8 peek-u8-port) #f)
            (guard (e (#t #t)) (u8-ready? peek-u8-port) #f)))
(close-input-port peek-u8-port)
(test "current-error-port output" #t (output-port? (current-error-port)))
(test "string port textual" #t (textual-port? (open-input-string "abc")))
(define compat-bvin (open-input-bytevector #u8(1 2 3)))
(test "bytevector input port?" #t (input-port? compat-bvin))
(test "bytevector input binary" #t (binary-port? compat-bvin))
(test "bytevector input not textual" #f (textual-port? compat-bvin))
(test "bytevector input open" #t (input-port-open? compat-bvin))
(test "bytevector input read-u8" 1 (read-u8 compat-bvin))
(close-input-port compat-bvin)
(test "bytevector input closed" #f (input-port-open? compat-bvin))
(test "open-input-bytevector range" '(20 30)
      (let ((p (open-input-bytevector #u8(10 20 30 40) 1 3)))
        (list (read-u8 p) (read-u8 p))))
(test "open-input-bytevector observes source mutation" #u8(9 2)
      (let ((source (bytevector 1 2 3)))
        (let ((p (open-input-bytevector source 0 2)))
          (bytevector-u8-set! source 0 9)
          (read-bytevector 2 p))))
(test "open-input-bytevector validates range" '(#t #t #t)
      (list
       (guard (e (#t #t)) (open-input-bytevector #u8(1) -1) #f)
       (guard (e (#t #t)) (open-input-bytevector #u8(1) 1 0) #f)
       (guard (e (#t #t)) (open-input-bytevector #u8(1) 0 2) #f)))
(test "closing already closed ports is idempotent" '(#t #t #t #t)
      (list
       (let ((p (open-input-string "x")))
         (close-input-port p)
         (guard (e (#t #f)) (close-input-port p) #t))
       (let ((p (open-output-string)))
         (close-output-port p)
         (guard (e (#t #f)) (close-output-port p) #t))
       (let ((p (open-input-bytevector #u8(1))))
         (close-input-port p)
         (guard (e (#t #f)) (close-input-port p) #t))
       (let ((p (open-output-bytevector)))
         (close-output-port p)
         (guard (e (#t #f)) (close-output-port p) #t))))
(test "malformed bytevector port vectors are rejected" '(#f #f)
      (list (bytevector-input-port? (vector 'bvin))
            (bytevector-output-port? (vector 'bvout))))
(test "mutated bytevector port state is rejected" '(#f #f)
      (let ((in (open-input-bytevector (bytevector 1)))
            (out (open-output-bytevector)))
        (vector-set! in 2 99)
        (vector-set! out 1 7)
        (list (bytevector-input-port? in)
              (bytevector-output-port? out))))
(test "open-input-bytevector validates its argument" #t
      (guard (e (#t #t))
        (open-input-bytevector "not a bytevector")
        #f))
(test "read-bytevector validates count on bytevector ports" #t
      (let ((p (open-input-bytevector (bytevector 1))))
        (guard (e (#t #t))
          (read-bytevector -1 p)
          #f)))
(test "read-bytevector zero at bytevector EOF returns empty" #u8()
      (let ((p (open-input-bytevector (bytevector 1))))
        (read-bytevector 1 p)
        (read-bytevector 0 p)))
(test "read-bytevector! bytevector input port" '(2 #u8(0 10 20 0))
      (let ((p (open-input-bytevector (bytevector 10 20 30)))
            (target (bytevector 0 0 0 0)))
        (list (read-bytevector! target p 1 3) target)))
(test "read-bytevector! honors empty target range and EOF"
      '(0 10 #t)
      (let ((p (open-input-bytevector (bytevector 10 20))))
        (list (read-bytevector! (make-bytevector 3) p 1 1)
              (read-u8 p)
              (eof-object?
               (read-bytevector! (make-bytevector 3)
                                 (open-input-bytevector (bytevector)))))))
(test "bytevector input port wrappers validate arity" '(#t #t)
      (let ((p (open-input-bytevector (bytevector 7))))
        (list (guard (e (#t #t)) (peek-u8 p p) #f)
              (guard (e (#t #t)) (u8-ready? p p) #f))))
(define compat-bvout (open-output-bytevector))
(test "bytevector output port?" #t (output-port? compat-bvout))
(test "bytevector output binary" #t (binary-port? compat-bvout))
(test "bytevector output not textual" #f (textual-port? compat-bvout))
(test "bytevector output open" #t (output-port-open? compat-bvout))
(write-bytevector #u8(65 66 67) compat-bvout 1 3)
(test "bytevector output result" #u8(66 67) (get-output-bytevector compat-bvout))
(test "get-output-bytevector works after close" #u8(66 67)
      (begin
        (close-output-port compat-bvout)
        (get-output-bytevector compat-bvout)))
(test "call-with-output-bytevector" #u8(65 255)
      (call-with-output-bytevector
        (lambda (p) (write-bytevector #u8(65 255) p))))
(test "bytevector port can be current output" #u8(65 10 51 53)
      (let ((old (current-output-port))
            (p (open-output-bytevector)))
        (set-current-output-port! p)
        (if (not (bytevector-output-port? (current-output-port)))
            (error "current bytevector output lost"))
        (display "A")
        (newline)
        (write 35)
        (set-current-output-port! old)
        (get-output-bytevector p)))
(test "bytevector port can be current input" '(7 8)
      (let ((old (current-input-port))
            (p (open-input-bytevector #u8(7 8))))
        (set-current-input-port! p)
        (let ((result (list (read-u8) (read-u8))))
          (set-current-input-port! old)
          result)))
(test "flush-output-port supports bytevector output ports" '(#t #t)
      (list
       (let ((p (open-output-bytevector)))
         (guard (e (#t #f)) (flush-output-port p) #t))
       (let ((p (open-output-bytevector)))
         (close-output-port p)
         (guard (e (#t #t)) (flush-output-port p) #f))))
(test "bytevector I/O validates arity"
      '(#t #t)
      (list
       (guard (e (#t #t))
         (write-bytevector #u8(1) compat-bvout 0 1 99)
         #f)
       (guard (e (#t #t))
         (read-bytevector 1 compat-bvin compat-bvin)
         #f)))
(close-output-port compat-bvout)
(test "bytevector output closed" #f (output-port-open? compat-bvout))
(test "string-split rejects excess arguments" #t
      (guard (e (#t #t))
        (string-split "a" #\space #\tab)
        #f))
(test "string-split validates an explicit delimiter on empty input" '(#t #t)
      (list (guard (e (#t #t)) (string-split "" 42) #f)
            (guard (e (#t #t)) (string-split "" #f) #f)))
(test "string-join rejects excess arguments" #t
      (guard (e (#t #t))
        (string-join '("a") " " "[" "]" "extra")
        #f))
(test "string-join validates finite string lists" '(#t #t)
      (let ((cycle (list "a" "b")))
        (set-cdr! (cdr cycle) cycle)
        (list
         (guard (e (#t #t))
           (string-join cycle)
           #f)
         (guard (e (#t #t))
           (string-join '("a" 2))
           #f))))

; File/OS helpers
(define compat-file "/tmp/vesper-compat-test.bin")
(define compat-out (open-binary-output-file compat-file))
(test "binary output port predicate" #t (binary-port? compat-out))
(test "binary output not textual" #f (textual-port? compat-out))
(write-bytevector (bytevector 65 66 67) compat-out)
(close-output-port compat-out)
(test "file-exists? compat file" #t (file-exists? compat-file))
(test "file-regular? compat file" #t (file-regular? compat-file))
(test "file-directory? compat file" #f (file-directory? compat-file))
(define compat-in (open-binary-input-file compat-file))
(test "binary input port predicate" #t (binary-port? compat-in))
(test "read-bytevector file" #u8(65 66 67) (read-bytevector 3 compat-in))
(close-input-port compat-in)
(define compat-in-slice (open-binary-input-file compat-file))
(define compat-target (bytevector 0 0 0 0 0))
(test "read-bytevector! range" 2 (read-bytevector! compat-target compat-in-slice 1 3))
(test "read-bytevector! range target" #u8(0 65 66 0 0) compat-target)
(close-input-port compat-in-slice)
(delete-file compat-file)
(test "delete-file compat file" #f (file-exists? compat-file))
(define compat-transcript-file "/tmp/vesper-compat-transcript.txt")
(transcript-on compat-transcript-file)
(display "transcript text")
(write #t)
(write-char #\!)
(write-string " more")
(newline)
(transcript-off)
(define compat-transcript-in (open-input-file compat-transcript-file))
(test "transcript captures output" "transcript text#t! more"
      (read-line compat-transcript-in))
(close-input-port compat-transcript-in)
(delete-file compat-transcript-file)
(define compat-text-file "/tmp/vesper-compat-text.txt")
(define compat-text-out (open-output-file compat-text-file))
(test "text output port predicate" #t (textual-port? compat-text-out))
(test "text output not binary" #f (binary-port? compat-text-out))
(close-output-port compat-text-out)
(delete-file compat-text-file)
(test "current-directory returns string" #t (string? (current-directory)))
(test "directory-files returns list" #t (list? (directory-files ".")))
(define compat-dir "/tmp/vesper-compat-dir")
(make-directory compat-dir)
(test "make-directory/file-directory?" #t (file-directory? compat-dir))
(delete-directory compat-dir)
(test "delete-directory" #f (file-exists? compat-dir))
(test "temporary-file-path"
      #t
      (let ((path (temporary-file-path)))
        (and (string? path) (not (file-exists? path)))))
(test "path-join" "/tmp/vesper/file.txt" (path-join "/tmp/" "vesper" "file.txt"))
(test "path-join root" "/vesper" (path-join "/" "vesper"))
(test "path-join repeated root" "/vesper" (path-join "/" "/" "vesper"))
(test "path-basename" "file.txt" (path-basename "/tmp/vesper/file.txt"))
(test "path-directory" "/tmp/vesper" (path-directory "/tmp/vesper/file.txt"))
(test "path-directory relative filename" "." (path-directory "file.txt"))
(test "path-extension" "txt" (path-extension "/tmp/vesper/file.txt"))
(test "path-with-extension" "/tmp/vesper/file.scm"
      (path-with-extension "/tmp/vesper/file.txt" "scm"))
(test "path-with-extension root" "/file.scm"
      (path-with-extension "/file.txt" "scm"))
(test "current-second inexact" #t (inexact? (current-second)))
(test "current-jiffy integer" #t (integer? (current-jiffy)))
(test "jiffies-per-second" 1000000000 (jiffies-per-second))
(test "get-environment-variables returns alist"
      #t
      (let ((envs (get-environment-variables)))
        (or (null? envs)
            (and (pair? (car envs))
                 (string? (caar envs))
                 (string? (cdar envs))))))
(test "character sets" '(#t #t #f #t #t #t)
      (let ((set (char-set #\a "bc" (cons 100 103))))
        (list (char-set? set)
              (char-in-set? #\a set)
              (char-in-set? #\z set)
              (code-point-in-set? 101 set)
              ((char-set-predicate set) #\c)
              (8-bit-char-set? set))))
(test "character set operations" '(#t #t #t #t)
      (let* ((letters (char-set #\a #\b))
             (digits (char-set #\1 #\2))
             (both (char-set-union letters digits))
             (only-a (char-set-difference letters (char-set #\b))))
        (list (char-in-set? #\1 both)
              (not (char-in-set? #\c both))
              (char-in-set? #\a only-a)
              (not (char-in-set? #\b only-a)))))
(test "character set algebra identities" '(#t #t #t #t)
      (list
       (not (char-in-set? #\a (char-set-union)))
       (char-in-set? #\a (char-set-intersection))
       (not (char-in-set? #\a (char-set-union* '())))
       (char-in-set? #\a (char-set-intersection* '()))))
(test "8-bit character set boundary" '(#t #f)
      (list
       (8-bit-char-set? (char-set (cons 0 256)))
       (8-bit-char-set? (char-set (cons 256 257)))))
(test "SRFI-14 character set conversions" '(#t #t #t #t)
      (let ((set (list->char-set (list #\a #\b #\a))))
        (list
         (char-set= set (string->char-set "ab"))
         (equal? (char-set->list set) (list #\a #\b))
         (equal? (char-set->string set) "ab")
         (char-set? (->char-set #\a)))))
(test "SRFI-14 character set updates" '(#t #f #t #t)
      (let* ((base (char-set #\a))
             (added (char-set-adjoin base #\b))
             (deleted (char-set-delete added #\a))
             (xor (char-set-xor added (char-set #\b))))
        (list (char-set-contains? added #\b)
              (char-set-contains? deleted #\a)
              (char-set-contains? deleted #\b)
              (char-set-contains? xor #\a))))
(test "SRFI-14 linear-update constructor wrappers" '(#t #t #t #t #t)
      (let ((base (char-set #\a)))
        (list (let ((set (list->char-set! '(#\b) base)))
                (and (char-set-contains? set #\a)
                     (char-set-contains? set #\b)))
              (let ((set (string->char-set! "c" base)))
                (and (char-set-contains? set #\a)
                     (char-set-contains? set #\c)))
              (let ((set (char-set-filter! char-alphabetic?
                                           (char-set #\a #\1) base)))
                (and (char-set-contains? set #\a)
                     (not (char-set-contains? set #\1))))
              (let ((set (char-set-unfold! (lambda (n) (> n 2))
                                           (lambda (n)
                                             (integer->char (+ 97 n)))
                                           1+ 0 base)))
                (and (char-set-contains? set #\a)
                     (char-set-contains? set #\b)
                     (char-set-contains? set #\c)))
              (let ((set (ucs-range->char-set! 120 123 #f base)))
                (and (char-set-contains? set #\a)
                     (char-set-contains? set #\x)
                     (char-set-contains? set #\z))))))
(test "SRFI-14 linear-update algebra wrappers" '(#t #t #t)
      (call-with-values
          (lambda ()
            (char-set-diff+intersection!
             (char-set #\a #\b) (char-set #\b #\c)))
        (lambda (difference intersection)
          (list (and (char-set-contains? difference #\a)
                     (not (char-set-contains? difference #\b)))
                (let ((set (char-set-difference!
                            (char-set #\a #\b) (char-set #\b))))
                  (and (char-set-contains? set #\a)
                       (not (char-set-contains? set #\b))))
                (let ((set (char-set-xor!
                            (char-set #\a) (char-set #\b))))
                  (and (char-set-contains? set #\a)
                       (char-set-contains? set #\b)))))))
(test "SRFI-14 character set queries" '(3 3 #t #\b #f #t)
      (let ((set (char-set #\a #\b #\c)))
        (list (char-set-size set)
              (char-set-count char-alphabetic? set)
              (char-set-every char-alphabetic? set)
              (char-set-any (lambda (char) (and (char=? char #\b) char)) set)
              (char-set<= set (char-set #\a #\b))
              (char-set= (char-set) (char-set)))))
(test "SRFI-14 character set constructors" '(#t #t #t #t #t)
      (let* ((base (char-set #\a))
             (filtered (char-set-filter char-alphabetic?
                                        (char-set #\1 #\b) base))
             (range (ucs-range->char-set 98 100))
             (unfolded (char-set-unfold (lambda (n) (> n 2))
                                        (lambda (n) (integer->char (+ 97 n)))
                                        (lambda (n) (+ n 1))
                                        0)))
        (list (char-set-contains? filtered #\a)
              (char-set-contains? filtered #\b)
              (char-set= range (char-set #\b #\c))
              (char-set? (char-set-copy range))
              (char-set-contains? unfolded #\c))))
(test "char-set-filter skips unused predicate on empty set" '(#t #t)
      (let ((empty (char-set-filter #f (char-set)))
            (with-base (char-set-filter #f (char-set)
                                        (char-set #\a))))
        (list (and (char-set? empty)
                   (not (char-in-set? #\a empty)))
              (and (char-in-set? #\a with-base)
                   (not (char-in-set? #\b with-base))))))
(test "char-set folds skip unused predicate on empty set" '(0 #t #f)
      (list (char-set-count #f (char-set))
            (char-set-every #f (char-set))
            (char-set-any #f (char-set))))
(test "char-set-fold skips unused reducer on empty set"
      17
      (char-set-fold #f 17 (char-set)))
(test "ucs-range bounds clamp without error"
      #t
      (char-in-set? #\A
                    (ucs-range->char-set 65
                                         1000000000000000000000000000000
                                         #f)))
(test "ucs-range invalid upper errors when requested"
      #t
      (guard (e (#t #t))
        (ucs-range->char-set 65 #x110001 #t)
        #f))
(test "SRFI-14 character set xor and partition" '(#t #t)
      (call-with-values
          (lambda ()
            (char-set-diff+intersection (char-set #\a #\b)
                                         (char-set #\b #\c)))
        (lambda (difference intersection)
          (list (and (char-set-contains? difference #\a)
                     (not (char-set-contains? difference #\b)))
                (and (char-set-contains? intersection #\b)
                     (not (char-set-contains? intersection #\a)))))))
(test "SRFI-14 character set iteration" '(#\a #\b #\c #t 3)
      (let* ((set (char-set "abc"))
             (cursor (char-set-cursor set))
             (next (char-set-cursor-next set cursor))
             (last (char-set-cursor-next set next))
             (end (char-set-cursor-next set last)))
        (list (char-set-ref set cursor)
              (char-set-ref set next)
              (char-set-ref set last)
              (end-of-char-set? end)
              (char-set-fold (lambda (char n) (+ n 1)) 0 set))))
(test "SRFI-14 standard character sets" '(#t #t #t #t #t)
      (list (char-set-contains? char-set:letter #\a)
            (char-set-contains? char-set:digit #\7)
            (char-set-contains? char-set:letter+digit #\7)
            (char-set-contains? char-set:ascii #\z)
            (char-set-contains? char-set:hex-digit #\F)))
(test "SRFI-14 extended standard character sets"
      '(#t #t #t #t #t #t #t #f #t #f)
      (list (char-set-contains? char-set:punctuation #\!)
            (char-set-contains? char-set:symbol #\+)
            (char-set-contains? char-set:graphic #\A)
            (char-set-contains? char-set:graphic #\!)
            (char-set-contains? char-set:printing #\space)
            (char-set-contains? char-set:iso-control (integer->char 0))
            (char-set-contains? char-set:title-case (integer->char #x01C5))
            (char-set-contains? char-set:title-case #\A)
            (char-set-contains? char-set:printing #\newline)
            (char-set-contains? char-set:graphic #\space)))
(test "SRFI-14 Unicode punctuation and symbols"
      '(#t #t #t #t)
      (list (char-set-contains? char-set:punctuation (integer->char #x2014))
            (char-set-contains? char-set:symbol (integer->char #x2603))
            (char-set-contains? char-set:graphic (integer->char #x2603))
            (char-set-contains? char-set:printing (integer->char #x2603))))
(test "SRFI-14 character set hash" #t
      (= (char-set-hash (char-set #\a #\b) 17)
         (char-set-hash (char-set #\b #\a) 17)))
(test "SRFI-14 character set hash rejects zero bound" #t
      (guard (e (#t #t))
        (char-set-hash (char-set #\a) 0)
        #f))
(test "SRFI-14 char-set-unfold preserves finite range storage" 3
      (char-set-size
       (char-set-unfold (lambda (x) (= x 3))
                        (lambda (x) (integer->char (+ 65 x)))
                        (lambda (x) (+ x 1))
                        0)))
(test "character set code points" '((97 . 100))
      (char-set->code-points (char-set #\a "bc")))
(test "malformed character sets are rejected"
      '(#f #f #t)
      (let ((bad-predicate (vector 'char-set 'predicate 42))
            (bad-ranges (vector 'char-set 'ranges (list 42)))
            (good (char-set "A")))
        (list (char-set? bad-predicate)
              (char-set? bad-ranges)
              (char-set? good))))
(test "char-set accepts code points" #t
      (char-in-set? #\* (char-set 42)))
(test "char-set rejects unknown names" #t
      (guard (exn (else #t))
        (char-set 'definitely-not-a-character-set)
        #f))
(test "unicode character set excludes surrogates"
      #t
      (not (code-point-in-set? #xD800 (char-set 'unicode))))
(test "unicode character set excludes unassigned code points"
      '(#f #f #t)
      (list (code-point-in-set? #x0378 (char-set 'unicode))
            (code-point-in-set? #x10FFFF (char-set 'unicode))
            (code-point-in-set? #xE000 (char-set 'unicode))))
(test "read-delimited-string preserves delimiter" '("abc" #\, #\, "def")
      (call-with-input-string "abc,def"
        (lambda (port)
          (let ((set (char-set #\,)))
            (list (read-delimited-string set port)
                  (peek-char port)
                  (read-char port)
                  (read-delimited-string set port))))))
(test "legacy delimiter read-string preserves delimiter" '("abc" #\, #\, "def")
      (call-with-input-string "abc,def"
        (lambda (port)
          (let ((set (char-set #\,)))
            (list (read-string set port)
                  (peek-char port)
                  (read-char port)
                  (read-string set port))))))
(test "discard-chars preserves delimiter" '(#f #\, #\,)
      (call-with-input-string "abc,def"
        (lambda (port)
          (let ((set (char-set #\,)))
            (list (discard-chars set port)
                  (peek-char port)
                  (read-char port))))))
(test "read-string! bounded and partial" '(3 2 "abcxy" #t)
      (let ((target (string-copy ".....")))
        (let ((count (call-with-input-string "abc"
                       (lambda (port) (read-string! target port 0 5)))))
          (list count
                (call-with-input-string "xy"
                  (lambda (port) (read-string! target port 3 5)))
                target
                (eof-object?
                 (call-with-input-string ""
                   (lambda (port) (read-string! target port 0 2))))))))
(test "read-substring! compatibility" "aZc"
      (let ((target (string-copy "abc")))
        (call-with-input-string "Z"
          (lambda (port)
            (read-substring! target 1 2 port)
            target))))
(test "read-string! zero range" 0
      (let ((target (string-copy "abc")))
        (call-with-input-string "x"
          (lambda (port) (read-string! target port 1 1)))))
(test "read-string! validates range" #t
      (guard (e (#t #t))
        (read-string! (string-copy "abc") (open-input-string "x") 2 1)
        #f))
(test "read-string! validates empty-range port" '(#t #t)
      (list (guard (e (#t #t))
              (read-string! (make-string 0) 42 0 0)
              #f)
            (guard (e (#t #t))
              (read-string! (make-string 0)
                            (open-input-bytevector (bytevector))
                            0 0)
              #f)))
(test "string-copy! rejects immutable empty destination" #t
      (guard (e (#t #t))
        (string-copy! "" 0 "" 0 0)
        #f))

(section "Additional R7RS Compatibility")

(test "boolean=? true" #t (boolean=? #t #t #t))
(test "boolean=? false" #f (boolean=? #t #t #f))
(test "boolean=? rejects non-boolean" #f (boolean=? #t 1))
(define call-with-port-result
  (let ((p (open-input-string "x")))
    (cons (call-with-port p read-char)
          (input-port-open? p))))
(test "call-with-port closes" (cons #\x #f) call-with-port-result)
(test "call-with-port preserves port on continuation escape" #t
      (let ((p (open-input-string "x")))
        (call/cc
          (lambda (k)
            (call-with-port p (lambda (port) (k #t)))))
        (let ((open? (input-port-open? p)))
          (close-input-port p)
          open?)))
(define close-port-output (open-output-string))
(test "port? output" #t (port? close-port-output))
(test "i/o-port? distinguishes directional ports" '(#f #f #f)
      (list (i/o-port? (open-input-string "x"))
            (i/o-port? (open-output-string))
            (i/o-port? 42)))
(close-port close-port-output)
(test "close-port closes output" #f (output-port-open? close-port-output))
(test "exact alias" #t (exact? (exact 1.0)))
(test "inexact alias" #t (inexact? (inexact 1)))
(test "exact-integer?" #t (exact-integer? 42))
(test "exact-integer? inexact" #f (exact-integer? 42.0))
(test "floor-quotient positive" 3 (floor-quotient 10 3))
(test "floor-remainder positive" 1 (floor-remainder 10 3))
(test "floor-quotient negative" -4 (floor-quotient -10 3))
(test "floor-remainder negative" 2 (floor-remainder -10 3))
(test "floor/ values" '(-4 2)
      (call-with-values (lambda () (floor/ -10 3)) list))
(test "truncate-quotient negative" -3 (truncate-quotient -10 3))
(test "truncate-remainder negative" -1 (truncate-remainder -10 3))
(test "truncate/ values" '(-3 -1)
      (call-with-values (lambda () (truncate/ -10 3)) list))
(test "truncate/ bignum values"
      '(-33333333333333333333 -1)
      (call-with-values
        (lambda () (truncate/ -100000000000000000000 3))
        list))
(test "floor/ bignum values"
      '(-33333333333333333334 2)
      (call-with-values
        (lambda () (floor/ -100000000000000000000 3))
        list))
(test "letrec*" 3
      (letrec* ((x 1)
                (y (+ x 2)))
        y))
(define list-set-target (list 'a 'b 'c))
(list-set! list-set-target 1 'x)
(test "list-set!" '(a x c) list-set-target)
(test "square" 49 (square 7))
(test "string->vector" '#(#\b #\c) (string->vector "abcd" 1 3))
(test "symbol=? true" #t (symbol=? 'a 'a 'a))
(test "symbol=? false" #f (symbol=? 'a 'a 'b))
(test "symbol=? rejects non-symbol" #f (symbol=? 'a 1))
(test "vector->string" "bc" (vector->string '#(#\a #\b #\c #\d) 1 3))
(test "format radix and char" "hex ff oct 377 bin 1010 char A"
      (format #f "hex ~x oct ~o bin ~b char ~c" 255 255 10 #\A))
(test "format fresh line" "a\nb" (format #f "a~&b"))
(test "format space directive" "a b" (format #f "a~_b"))
(test "write-simple" "\"x\"" (call-with-output-string
                                (lambda (p) (write-simple "x" p))))
(test "write-shared" "(1 2)" (call-with-output-string
                                (lambda (p) (write-shared '(1 2) p))))
(test "write-shared emits labels" "(#0=(1) #0#)"
      (call-with-output-string
        (lambda (p)
          (let ((x (list 1)))
            (write-shared (list x x) p)))))
(test "write-simple repeats shared data" "((1) (1))"
      (call-with-output-string
        (lambda (p)
          (let ((x (list 1)))
            (write-simple (list x x) p)))))
(test "exact-nonnegative-integer?" #t (exact-nonnegative-integer? 0))
(test "exact-rational?" #t (exact-rational? 1/3))
(test "exact-integer-sqrt" '(5 2)
      (call-with-values (lambda () (exact-integer-sqrt 27)) list))
(test "euclidean/" '(4 3)
      (call-with-values (lambda () (euclidean/ -13 -4)) list))
(test "ceiling/" '(4 -3)
      (call-with-values (lambda () (ceiling/ 13 4)) list))
(test "round/" '(4 -1)
      (call-with-values (lambda () (round/ 15 4)) list))
(test "integer-divide" '(-3 -1)
      (let ((qr (integer-divide -10 3)))
        (list (integer-divide-quotient qr)
              (integer-divide-remainder qr))))
(test "modexp" 46 (modexp 1234 5678 90))
(test "modexp validates exact integer domain and preserves zero exponent"
      '(1 #t #t #t #t)
      (list (modexp 2 0 1)
            (guard (e (#t #t)) (modexp 2.0 3 5) #f)
            (guard (e (#t #t)) (modexp 2 3.0 5) #f)
            (guard (e (#t #t)) (modexp 2 3 -5) #f)
            (guard (e (#t #t)) (modexp 2 -1 5) #f)))
(test "floor->exact" #t (exact? (floor->exact 3.7)))
(test "logistic" 0.5 (logistic 0))
(test "logsumexp rejects circular input" #t
      (let ((cycle (circular-list 0.0 1.0)))
        (guard (e (#t #t))
          (logsumexp cycle)
          #f)))
(test "conjugate" -2 (imag-part (conjugate (make-rectangular 1 2))))
(test "stable log1p tiny" #t (< (abs (- (log1p 1e-20) 1e-20)) 1e-30))
(test "stable expm1 tiny" #t (< (abs (- (expm1 1e-20) 1e-20)) 1e-30))
(test "stable sqrt1pm1 tiny" #t (< (abs (- (sqrt1pm1 1e-20) 5e-21)) 1e-30))
(test "stable log1pexp large" 1000.0 (log1pexp 1000.0))
(test "complex stable numerical helpers" #t
      (and (complex? (log1p 1+2i))
           (complex? (expm1 1+2i))
           (complex? (sqrt1pm1 1+2i))
           (complex? (log1pexp 1+2i))))
(test "log1p real branch cut" #t
      (and (complex? (log1p -2))
           (= (real-part (log1p -2)) 0.0)))
(test "sqrt1pm1 real branch cut" #t
      (and (complex? (sqrt1pm1 -2))
           (= (real-part (sqrt1pm1 -2)) -1.0)
           (= (imag-part (sqrt1pm1 -2)) 1.0)))
(test "log1pexp large complex finite" #t
      (let ((z (log1pexp 1000+1i)))
        (and (finite? (real-part z))
             (finite? (imag-part z))
             (< (abs (- (real-part z) 1000.0)) 1e-12)
             (< (abs (- (imag-part z) 1.0)) 1e-12))))
(test "digit-value rejects alphabetic digit syntax" #f (digit-value #\f))
(test "unicode digit-value" 5 (digit-value (integer->char #x0665)))
(test "unicode digit-value extended arabic-indic" 7
      (digit-value (integer->char #x06F7)))
(test "unicode digit-value fullwidth" 9
      (digit-value (integer->char #xFF19)))
(test "unicode digit-value newer ranges" '(3 5 0)
      (list (digit-value (integer->char #x07C3))
            (digit-value (integer->char #x1D7D3))
            (digit-value (integer->char #x1E140))))
(test "digit-value rejects alphabetic characters" #f
      (digit-value #\A))
(test "character digit conversions" '(#t #t #f #\E #\9)
      (list (char-alphanumeric? #\a)
            (= (char->digit #\E 16) 14)
            (char->digit #\9 8)
            (digit->char 14 16)
            (digit->char 9)))
(test "char->digit accepts Unicode decimal digits"
      '(5 9 5)
      (list (char->digit (integer->char #x0665))
            (char->digit (integer->char #xFF19))
            (char->digit (integer->char #x1D7D3))))
(test "character digit conversion validation" #t
      (guard (e (#t #t))
        (digit->char 10 10)
        #f))
(test "character names" '("space" "A" #\space #\A #\λ)
      (list (char->name #\space)
            (char->name #\A)
            (name->char "SPACE")
            (name->char "A")
            (name->char (char->name #\λ))))
(test "name->char null" #\null (name->char "null"))
(test "character name validation" #t
      (guard (e (#t #t))
        (name->char "not-a-character")
        #f))
(test "char-foldcase" #\a (char-foldcase #\A))
(test "string-foldcase" "abc" (string-foldcase "AbC"))
(test "unicode char predicates" #t
      (and (char-alphabetic? (integer->char #x03bb))
           (char-upper-case? (integer->char #x0391))
           (char-lower-case? (integer->char #x03b1))
           (char-whitespace? (integer->char #x2003))))
(test "unicode char-foldcase final sigma" (integer->char #x03c3)
      (char-foldcase (integer->char #x03c2)))
(test "unicode string-upcase expands" "SSFFI" (string-upcase "ßﬃ"))
(test "unicode string-downcase final sigma"
      (unicode-char #x03bf #x03c2)
      (string-downcase (unicode-char #x039f #x03a3)))
(test "unicode string-titlecase final sigma"
      (unicode-char #x039f #x03c2)
      (string-titlecase (unicode-char #x039f #x03a3)))
(test "unicode string-foldcase expands" "ssi̇ksμσσσffi"
      (string-foldcase "ẞİKſµΣςσﬃ"))
(test "string transformations return immutable strings" '(#t #t #t #t)
      (list
       (guard (e (#t #t))
         (let ((s (string-upcase "a"))) (string-set! s 0 #\x) #f))
       (guard (e (#t #t))
         (let ((s (string-downcase "A"))) (string-set! s 0 #\x) #f))
       (guard (e (#t #t))
         (let ((s (string-foldcase "A"))) (string-set! s 0 #\x) #f))
       (guard (e (#t #t))
         (let ((s (string-normalize-nfc "a"))) (string-set! s 0 #\x) #f))))
(test "unicode string-ci uses full foldcase" #t
      (and (string-ci=? "straße" "STRASSE")
           (string-ci=? (unicode-char #x0399 #x03A0 #x03A0 #x039F #x03A3)
                        (unicode-char #x03B9 #x03C0 #x03C0 #x03BF #x03C3))))
(test "unicode normalize nfd decomposes latin accent"
      (unicode-char 101 769)
      (string-normalize-nfd (unicode-char 233)))
(test "unicode normalize nfc composes latin accent"
      (unicode-char 233)
      (string-normalize-nfc (unicode-char 101 769)))
(test "unicode normalize orders combining marks"
      (unicode-char 97 807 769)
      (string-normalize-nfd (unicode-char 97 769 807)))
(test "unicode normalize nfkd expands ligature"
      "ffi"
      (string-normalize-nfkd (string (integer->char 64259))))
(test "unicode normalize nfkc fullwidth"
      "ABC123"
      (string-normalize-nfkc (unicode-char 65313 65314 65315 65297 65298 65299)))
(test "unicode normalized nfc predicate" #t
      (and (string-normalized-nfc? (unicode-char 233))
           (not (string-normalized-nfc? (unicode-char 101 769)))))
(test "MIT normalization aliases" #t
      (and (string=? (string->nfc (unicode-char 101 769)) (unicode-char 233))
           (string-in-nfc? (unicode-char 233))
           (string-in-nfd? (unicode-char 101 769))))
(test "features contains unicode tables" #t
      (and (not (not (memq 'unicode-normalization (features))))
           (not (not (memq 'unicode-case-folding (features))))
           (not (not (memq 'unicode-character-properties (features))))))
(test "utf8 roundtrip" "abc" (utf8->string (string->utf8 "abc")))
(test "utf8 encodes two-byte character" #u8(195 169)
      (string->utf8 (string (integer->char 233))))
(test "utf8 decodes two-byte character" (string (integer->char 233))
      (utf8->string #u8(195 169)))
(test "string->utf8 range" #u8(195 169 240 157 132 158)
      (string->utf8 (unicode-char 97 233 119070) 1 3))
(test "utf8->string range" (string (integer->char 233))
      (utf8->string #u8(65 195 169 66) 1 3))
(test "environment returns environment" #t (pair? (environment '(scheme base))))
(test "scheme-report-environment includes standard library"
      '(2 3)
      (let ((report (scheme-report-environment 5)))
        (list (eval '(length (map (lambda (x) (+ x 1)) '(1 2))) report)
              (eval '(+ 1 2) (scheme-report-environment 5)))))
(test "environment imports isolate interaction bindings"
      '(#t #t)
      (begin
        (define environment-private-binding 9173)
        (list
         (= (eval '(+ 2 3) (environment '(scheme base))) 5)
         (guard (e (#t #t))
           (eval 'environment-private-binding (environment '(scheme base)))
           #f))))
(test "environment imports only requested library exports"
      #t
      (let ((bound?
             (lambda (name spec)
               (guard (e (else #f))
                 (procedure? (eval name (environment spec)))))))
        (and (bound? '+ '(scheme base))
             (not (bound? 'write '(scheme base)))
             (bound? 'write '(scheme write))
             (not (bound? '+ '(scheme write)))
             (bound? 'sqrt '(scheme inexact))
             (not (bound? '+ '(scheme inexact)))
             (bound? 'magnitude '(scheme complex))
             (not (bound? '+ '(scheme complex)))
             (bound? 'cadddr '(scheme cxr))
             (not (bound? 'caaaar '(scheme base))))))
(test "environment import-set modifiers"
      #t
      (let ((bound?
             (lambda (name spec)
               (guard (e (else #f))
                 (procedure? (eval name (environment spec)))))))
        (and (bound? '+ '(only (scheme base) +))
             (not (bound? '- '(only (scheme base) +)))
             (not (bound? '+ '(except (scheme base) +)))
             (bound? '- '(except (scheme base) +))
             (bound? 'p:+ '(prefix (scheme base) p:))
             (not (bound? '+ '(prefix (scheme base) p:)))
             (bound? 'add '(rename (scheme base) (+ add)))
             (not (bound? '+ '(rename (scheme base) (+ add)))))))
(test "environment imports are immutable"
      #t
      (and
       (guard (e (else #t))
         (eval '(define environment-immutable-test 1)
               (environment '(scheme base)))
         #f)
       (guard (e (else #t))
         (eval '(set! + (lambda args 0))
               (environment '(scheme base)))
         #f)))
(test "environment with no imports is empty"
      #t
      (guard (e (#t #t))
        (eval '(+ 1 2) (environment))
        #f))
(test "environment rejects unknown library"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (environment '(scheme imaginary))))
(test "environment rejects malformed import modifiers"
      '(#t #t #t)
      (list
       (guard (exn (else #t))
         (environment '(only (scheme base)))
         #f)
       (guard (exn (else #t))
         (environment '(prefix (scheme base)))
         #f)
       (guard (exn (else #t))
         (environment '(rename (scheme unknown) (+ add)))
         #f)))
(test "environment rejects unknown import identifiers"
      '(#t #t #t)
      (list
       (guard (exn (else #t))
         (environment '(only (scheme base) definitely-not-exported))
         #f)
       (guard (exn (else #t))
         (environment '(except (scheme base) definitely-not-exported))
         #f)
       (guard (exn (else #t))
         (environment '(rename (scheme base) (definitely-not-exported renamed)))
         #f)))
(define-library (compat smoke)
  (export define-library-value)
  (import (scheme base))
  (begin
    (define define-library-value 91)))
(test "define-library begin" 91 define-library-value)
(test "error object guard"
      '("boom" (1 2))
      (guard (exn
              ((error-object? exn)
               (list (error-object-message exn)
                     (error-object-irritants exn))))
        (error "boom" 1 2)))
(test "raise-continuable returns handler value"
      'returned
      (with-exception-handler
        (lambda (exn) 'returned)
        (lambda () (raise-continuable 'continuable))))
(test "syntax-error object kind"
      #t
      (guard (exn
              ((read-error? exn) #t)
              (else #f))
        (syntax-error "bad syntax")))
(test "file-error? structured object"
      #t
      (file-error? (make-error-object 'file "missing" '("x"))))
(define include-test-file "/tmp/vesper-include-test.scm")
(define include-test-out (open-output-file include-test-file))
(write-string "(define included-value 77)" include-test-out)
(close-output-port include-test-out)
(include "/tmp/vesper-include-test.scm")
(test "include" 77 included-value)
(delete-file include-test-file)
(define-record-type record-collision
  (make-record-a x)
  record-a?
  (x record-a-x set-record-a-x!))
(define-record-type record-collision
  (make-record-b x)
  record-b?
  (x record-b-x))
(define-record-type reordered-record
  (make-reordered y x)
  reordered-record?
  (x reordered-x)
  (y reordered-y))
(define-record-type partial-record
  (make-partial y)
  partial-record?
  (x partial-x)
  (y partial-y))
(define record-a-instance (make-record-a 1))
(define record-b-instance (make-record-b 2))
(define reordered-record-instance (make-reordered 20 10))
(define partial-record-instance (make-partial 30))
(test "record unique tag" #f (record-a? record-b-instance))
(test "record accessor" 1 (record-a-x record-a-instance))
(set-record-a-x! record-a-instance 3)
(test "record mutator" 3 (record-a-x record-a-instance))
(test "record constructor reordered fields" '(10 20)
      (list (reordered-x reordered-record-instance)
            (reordered-y reordered-record-instance)))
(test "record constructor omitted field" #f
      (partial-x partial-record-instance))
(test "record constructor included field" 30
      (partial-y partial-record-instance))
(test "record accessor rejects wrong type"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (record-a-x record-b-instance)))
(test "record rejects duplicate fields"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x)
            bad-record?
            (x bad-x)
            (x bad-x2))
          #f)))
(test "record rejects duplicate constructor fields"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x x)
            bad-record?
            (x bad-x))
          #f)))
(test "record rejects unknown constructor field"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record y)
            bad-record?
            (x bad-x))
          #f)))
(test "record rejects duplicate generated binding"
      #t
      (guard (exn
              ((error-object? exn) #t)
              (else #f))
        (let ()
          (define-record-type bad-record
            (make-bad-record x)
            bad-record?
            (x make-bad-record))
          #f)))

(section "Vector/String Additions")

(test "vector-copy" '#(2 3) (vector-copy '#(1 2 3 4) 1 3))
(define copy-target (vector 0 0 0 0))
(vector-copy! copy-target 1 '#(8 9))
(test "vector-copy!" '#(0 8 9 0) copy-target)
(test "vector-copy arity" '(#t #t)
      (list
       (guard (e (#t #t)) (vector-copy '#(1) 0 1 2) #f)
       (guard (e (#t #t)) (vector-copy! (vector 0) 0 '#(1) 0 1 2) #f)))
(test "vector-copy validates ranges" '(#t #t #t #t #t #t)
      (list
       (guard (e (#t #t)) (vector-copy '#(1 2) -1 1) #f)
       (guard (e (#t #t)) (vector-copy '#(1 2) 2 1) #f)
       (guard (e (#t #t)) (vector-copy '#(1 2) 0 3) #f)
       (guard (e (#t #t)) (vector-copy! (vector 0) 2 '#(1) 0 1) #f)
       (guard (e (#t #t)) (vector-copy! (vector 0) 0 '#(1 2) 0 2) #f)
       (guard (e (#t #t)) (vector-copy! (vector 0) 0 42 0 0) #f)))
(test "vector-append" '#(1 2 3 4) (vector-append '#(1 2) '#(3 4)))
(test "vector-map" '#(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6)))
(define vector-for-each-sum 0)
(vector-for-each (lambda (x) (set! vector-for-each-sum (+ vector-for-each-sum x)))
                 '#(1 2 3))
(test "vector-for-each" 6 vector-for-each-sum)
(test "string-map" "bcd"
      (string-map (lambda (ch) (integer->char (+ 1 (char->integer ch)))) "abc"))
(define string-for-each-sum 0)
(string-for-each
  (lambda (ch)
    (set! string-for-each-sum (+ string-for-each-sum (char->integer ch))))
  "ab")
(test "string-for-each" (+ (char->integer #\a) (char->integer #\b))
      string-for-each-sum)
(define string-copy-target (string-copy "xxxx"))
(string-copy! string-copy-target 1 "abcd" 1 3)
(test "string-copy!" "xbcx" string-copy-target)
(test "string-copy! validates ranges" '(#t #t #t #t #t #t)
      (list
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 0 "xy" -1 1) #f)
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 0 "xy" 2 1) #f)
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 0 "xy" 0 3) #f)
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 3 "x") #f)
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 1 "xyz") #f)
       (guard (e (#t #t)) (string-copy! (string-copy "ab") 0 42) #f)))
(test "string/vector conversion validates arguments" '(#t #t #t #t #t #t #t)
      (list
       (guard (e (#t #t)) (string-copy! (string-copy "a") 0 "a" 0 1 2) #f)
       (guard (e (#t #t)) (string->vector "a" 0 1 2) #f)
       (guard (e (#t #t)) (vector->string #(#\a) 0 1 2) #f)
       (guard (e (#t #t)) (string->vector "a" -1) #f)
       (guard (e (#t #t)) (string->vector "a" 0 2) #f)
       (guard (e (#t #t)) (vector->string #(#\a) 1.0) #f)
       (guard (e (#t #t)) (vector->string #(#\a) 0 2) #f)))
(define unicode-string "aé𝄞")
(test "utf8 string-length counts characters" 3 (string-length unicode-string))
(test "utf8 string->list decodes characters" '(97 233 119070)
      (map char->integer (string->list unicode-string)))
(test "utf8 string-ref uses character index" 119070
      (char->integer (string-ref unicode-string 2)))
(test "utf8 substring uses character indexes" "é𝄞"
      (substring unicode-string 1 3))
(test "utf8 string-set! can change byte width"
      "a𝄞𝄞"
      (let ((s (string-copy unicode-string)))
        (string-set! s 1 (integer->char 119070))
        s))
(test "utf8 string-fill! range"
      "aéé"
      (let ((s (string-copy unicode-string)))
        (string-fill! s (integer->char 233) 1 3)
        s))
(test "utf8 escape string literal" 119070
      (char->integer (string-ref "\x1D11E;" 0)))
(test "unicode character hex literal" 119070
      (char->integer #\x1D11E))
(test "unicode character utf8 literal" 955
      (char->integer #\λ))
(test "unicode write char roundtrip" "#\\x1D11E"
      (write-to-string (integer->char #x1D11E)))
(test "unicode read written char roundtrip" 119070
      (char->integer (read (open-input-string
                            (write-to-string (integer->char #x1D11E))))))
(test "unicode identifier preserves utf8" 42
      (let ((λ 42)) λ))
(test "unicode identifier folds ascii and preserves lowercase unicode" 7
      (let ((fooλ 7)) Fooλ))
(test "unicode identifier foldcase" 12
      (let ((straße 12)) STRAẞE))
(test "unicode identifier foldcase expands ligature" 13
      (let ((office 13)) oﬃce))
(test "reader no-fold-case directive" "FOO"
      (symbol->string (read (open-input-string "#!no-fold-case FOO"))))
(test "reader no-fold-case preserves unicode" "ẞİKſµΣςσﬃ"
      (symbol->string (read (open-input-string "#!no-fold-case ẞİKſµΣςσﬃ"))))
(test "reader fold-case directive" "foo"
      (symbol->string (read (open-input-string "#!fold-case FOO"))))
(test "reader unicode foldcase directive" "ssi̇ksμσσσffi"
      (symbol->string (read (open-input-string "#!fold-case ẞİKſµΣςσﬃ"))))
(test "reader case directive toggles" '("FOO" "bar")
      (let ((p (open-input-string "#!no-fold-case FOO #!fold-case BAR")))
        (list (symbol->string (read p)) (symbol->string (read p)))))
(test "reader datum comment" 'kept
      (read (open-input-string "#;(discard (nested datum)) kept")))
(test "reader nested block comment" 'kept
      (read (open-input-string "#| outer #| inner |# done |# kept")))
(test "reader exactness/radix prefix" 16.0
      (read (open-input-string "#i#x10")))
(test "reader exact decimal" 3/2
      (read (open-input-string "#e1.50")))
(test "reader exact decimal exponent" 125
      (read (open-input-string "#e1.25e2")))
(test "escaped identifier preserves case and spaces" 9
      (let ((|Hello World| 9)) |Hello World|))
(test "escaped identifier scalar escape" 11
      (let ((|\x3BB;| 11)) λ))
(test "write escaped symbol roundtrip" "Hello World"
      (symbol->string (read (open-input-string
                             (write-to-string
                              (string->symbol "Hello World"))))))
(test "string-append*" "abc" (string-append* '("a" "b" "c")))
(test "string-append* validates finite string lists" '(#t #t)
      (let ((cycle (list "a" "b")))
        (set-cdr! (cdr cycle) cycle)
        (list
         (guard (e (#t #t))
           (string-append* cycle)
           #f)
         (guard (e (#t #t))
           (string-append* '("a" 2))
           #f))))
(test "string*" "a12#t" (string* (list "a" 12 #t)))
(test "string* rejects circular object lists" #t
      (let ((cycle (list "a" 2)))
        (set-cdr! (cdr cycle) cycle)
        (guard (e (#t #t))
          (string* cycle)
          #f)))
(test "string-compare" 'lt
      (string-compare "a" "b" (lambda () 'eq) (lambda () 'lt) (lambda () 'gt)))
(test "string-upper-case?" #t (string-upper-case? "ABC"))
(test "string-lower-case?" #f (string-lower-case? "Abc"))
(test "string-upper-case? ignores non-letters" #t (string-upper-case? "112"))
(test "string-lower-case? ignores non-letters" #t (string-lower-case? "112"))
(test "string-count" 3 (string-count char-alphabetic? "abc123"))
(test "string-any" #\b (string-any (lambda (c) (and (char=? c #\b) c)) "abc"))
(test "string-every" #t (string-every char-alphabetic? "abc"))
(test "string-null?" #t (string-null? ""))
(test "string-head" "ab" (string-head "abcd" 2))
(test "string-tail" "cd" (string-tail "abcd" 2))
(test "string-hash matches MIT FNV-1a" '(440920331 41 52 59 25)
      (list (string-hash "abc")
            (string-hash "abc" 97)
            (string-hash "é" 97)
            (string-hash "λ" 97)
            (string-hash "𝄞" 97)))
(test "string-hash modulus" #t (< (string-hash "abc" 10) 10))
(test "string-ci-hash alias" #t
      (= (string-ci-hash "ABC") (string-hash-ci "abc")))
(test "string-hash validates modulus"
      '(#t #t #t #t #t #t)
      (list
       (guard (e (#t #t)) (string-hash "abc" 0) #f)
       (guard (e (#t #t)) (string-hash "abc" -1) #f)
       (guard (e (#t #t)) (string-hash "abc" 1.5) #f)
       (guard (e (#t #t)) (string-hash "abc" #f) #f)
       (guard (e (#t #t)) (string-hash-ci "abc" #f) #f)
       (guard (e (#t #t)) (string-hash "abc" 10 20) #f)))
(define builder (string-builder))
(builder #\a)
(builder "bé")
(test "string-builder count" 3 (builder 'count))
(test "string-builder value" "abé" (builder))
(test "string-builder handles many pieces" 2000
      (string-length
       (let ((b (string-builder)))
         (let loop ((i 0))
           (if (= i 2000)
               (b)
               (begin (b #\x) (loop (+ i 1))))))))
(test "string-builder nfc" "é"
      (let ((b (string-builder)))
        (b "é")
        (b 'nfc)))
(builder 'reset!)
(test "string-builder empty" #t (builder 'empty?))
(test "string-builder validates buffer length"
      '(#t #t #t)
      (list
       (guard (e (#t #t)) (string-builder 0) #f)
       (guard (e (#t #t)) (string-builder -1) #f)
       (guard (e (#t #t)) (string-builder 1 2) #f)))
(test "string-builder validates command arity" #t
      (guard (e (#t #t)) ((string-builder) 'count 'count) #f))
(test "string keyword APIs validate options"
      '(#t #t #t #t)
      (list
       (guard (e (#t #t)) (string-joiner 'infix "," 'infix ";") #f)
       (guard (e (#t #t)) (string-splitter 'unknown #t) #f)
       (guard (e (#t #t)) (string-padder 'where 'leading 'where 'trailing) #f)
       (guard (e (#t #t)) (string-trimmer 'to-trim char-whitespace? 'bad #t) #f)))
(test "string-trimmer validates options" '(#t #t #t)
      (list
       (guard (e (#t #t)) (string-trimmer 'where 'bad) #f)
       (guard (e (#t #t)) (string-trimmer 'copy? 1) #f)
       (guard (e (#t #t)) (string-trimmer 'copier 1) #f)))
(test "string-splitter validates options" '(#t #t #t)
      (list
       (guard (e (#t #t)) (string-splitter 'allow-runs? 1) #f)
       (guard (e (#t #t)) (string-splitter 'copy? 1) #f)
       (guard (e (#t #t)) (string-splitter 'copier 1) #f)))
(test "string-join default separator" "a b" (string-join '("a" "b")))
(test "string-join explicit separator" "a,b" (string-join '("a" "b") ","))
(test "string-join prefix suffix" "[a,b]" (string-join '("a" "b") "," "[" "]"))
(test "string-join validates optional strings eagerly" '(#t #t #t)
      (list
       (guard (e (#t #t)) (string-join '() 42) #f)
       (guard (e (#t #t)) (string-join '() " " 42) #f)
       (guard (e (#t #t)) (string-join '() " " "" 42) #f)))
(test "string-join scales across many components" 1999
      (string-length (string-join (make-list 1000 "x") ",")))
(test "string-joiner" "<a|b>" ((string-joiner 'infix "|" 'prefix "<" 'suffix ">") "a" "b"))
(test "string-joiner validates keyword strings" #t
      (guard (e (#t #t)) (string-joiner 'infix 42) #f))
(test "string-joiner*" "<a|b>" ((string-joiner* 'infix "|" 'prefix "<" 'suffix ">") '("a" "b")))
(test "string-splitter runs" '("a" "b") ((string-splitter 'delimiter #\,) "a,,b"))
(test "string-splitter no runs" '("a" "" "b")
      ((string-splitter 'delimiter #\, 'allow-runs? #f) "a,,b"))
(test "string-splitter character-set delimiter" '("a" "b" "c")
      ((string-splitter 'delimiter (char-set #\space #\tab))
       "a b\tc"))
(test "string-splitter validates explicit copier before copy override" #t
      (guard (e (#t #t))
        ((string-splitter 'copy? #t 'copier 42) "a b")
        #f))
(test "string-pad-left" "..abc" (string-pad-left "abc" 5 #\.))
(test "string-pad-right" "abc.." (string-pad-right "abc" 5 #\.))
(test "string-padder validates arguments" '(#t #t #t #t)
      (list
       (guard (e (#t #t)) ((string-padder) "a" 1.0) #f)
       (guard (e (#t #t)) ((string-padder) "a" -1) #f)
       (guard (e (#t #t)) (string-padder 'where 'bad) #f)
       (guard (e (#t #t)) (string-padder 'fill-with "") #f)))
(test "string-padder rejects multi-character fill" #t
      (guard (e (#t #t))
        (string-padder 'fill-with "ab")
        #f))
(test "string-padder handles combining graphemes"
      '("ééx" "éx" "xéé")
      (list ((string-padder 'fill-with "é") "x" 3)
            ((string-padder) "éx" 2)
            ((string-padder 'where 'trailing 'fill-with "é") "x" 3)))
(test "string-padder handles extended graphemes"
      '("☝️☝️x" "👩‍💻👩‍💻x" "🇺🇸🇺🇸x" "👩‍💻x")
      (list ((string-padder 'fill-with "☝️") "x" 3)
            ((string-padder 'fill-with "👩‍💻") "x" 3)
            ((string-padder 'fill-with "🇺🇸") "x" 3)
            ((string-padder) "👩‍💻x" 2)))
(test "string-trimmer" "abc" ((string-trimmer) "  abc  "))
(test "string-trimmer character-set" "th"
      ((string-trimmer 'to-trim char-set:numeric) "100th"))
(test "string-trim character-set" "100"
      (string-trim "100th" char-set:numeric))
(test "string-trim-left/right character-set" '("100th" "100")
      (list (string-trim-left "100th" char-set:numeric)
            (string-trim-right "100th" char-set:numeric)))
(test "string-replace" "bonono" (string-replace "banana" #\a #\o))
(test "string-replace validates characters eagerly" '(#t #t #t)
      (list (guard (e (#t #t)) (string-replace "" 42 #\x) #f)
            (guard (e (#t #t)) (string-replace "" #\x 42) #f)
            (guard (e (#t #t)) (string-replace "abc" #\x 42) #f)))
(test "string-slice" "é𝄞" (string-slice unicode-string 1 3))
(test "string-slice shares mutable storage" '("azyde" "zyd" "azyde" "zyd")
      (let* ((original (string-copy "abcde"))
             (slice (string-slice original 1 4)))
        (string-set! slice 1 #\y)
        (let ((after-slice (list original slice)))
          (string-set! original 1 #\z)
          (list (car after-slice) (cadr after-slice)
                original slice))))
(test "string-slice fill shares mutable storage" '("aXXde" "XX")
      (let* ((original (string-copy "abcde"))
             (slice (string-slice original 1 3)))
        (string-fill! slice #\X)
        (list original slice)))
(test "string-slice immutable source" #t
      (let ((slice (string-slice "abcde" 1 4)))
        (guard (e (#t #t))
          (string-set! slice 0 #\x)
          #f)))
(test "nested string-slices share storage" '("abxdef" "bxd" "xd")
      (let* ((original (string-copy "abcdef"))
             (slice (string-slice original 1 4))
             (nested (string-slice slice 1 3)))
        (string-set! nested 0 #\x)
        (list original slice nested)))
(test "string-slice handles UTF-8 re-encoding" '("ax𝄞z" "x𝄞")
      (let* ((original (string-copy "aé𝄞z"))
             (slice (string-slice original 1 3)))
        (string-set! slice 0 #\x)
        (list original slice)))
(test "string-slice retains parent through GC" "bcde"
      (let ((slice (let ((original (string-copy "abcdef")))
                     (string-slice original 1 5))))
        (do ((i 0 (+ i 1)))
            ((= i 10000) slice)
          (make-vector 10 i))))
(test "string-slice parent survives allocation at creation" "zcde"
      (let ((original (string-copy "abcdef")))
        (do ((i 0 (+ i 1)))
            ((= i 10000)
             (let ((slice (string-slice original 1 5)))
               (string-set! original 1 #\z)
               slice))
          (make-vector 10 i))))

;;; ============================================================================
;;; gc-flip
;;; ============================================================================
;; PGCFLIP was absent from apply_primitive_argv's dispatch, so gc-flip was dead
;; in the default bytecode VM ("unknown primitive: 66") while working under
;; --interpreter, which intercepts it earlier. Nothing covered it. These run in
;; both modes, so they pin the two engines together.
(test "gc-flip returns true" #t (gc-flip))
(test "gc-flip rejects arguments" #t
      (guard (e (#t #t)) (gc-flip 1) #f))
(test "values survive an explicit gc-flip" '(1 2 3)
      (let ((live (list 1 2 3)))
        (gc-flip)
        live))
(test "gc-flip reclaims and leaves the heap usable" 4950
      (begin
        (do ((i 0 (+ i 1))) ((= i 200)) (make-vector 50 i))
        (gc-flip)
        (let loop ((i 0) (acc 0))
          (if (= i 100) acc (loop (+ i 1) (+ acc i))))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

(if (= fail-count 0)
    (begin
      (display "All tests passed!")
      (newline))
    (begin
      (display "SOME TESTS FAILED!")
      (newline)
      ;; Exit non-zero so the build actually fails, matching property_tests.scm
      ;; and stress_tests.scm. Without this, `make test` reported success on a
      ;; failing suite: its recipe took grep's exit status, and the pattern
      ;; matches the PASS lines that are always present.
      (exit 1)))
