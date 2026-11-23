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
(test "min ignores NaN in either order; signed zeros keep operand order"
      '(#f #f #t #f)
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
(test "membership searches stop before improper tails"
      '((2 . tail) (2 . tail) (2 . tail) (2 . tail))
      (list (memq 2 '(1 2 . tail))
            (memv 2 '(1 2 . tail))
            (member 2 '(1 2 . tail))
            ((member-procedure =) 2 '(1 2 . tail))))
(test "membership searches reject an improper tail when reached"
      '(#t #t #t #t)
      (list (guard (e (#t #t)) (memq 2 '(1 . tail)) #f)
            (guard (e (#t #t)) (memv 2 '(1 . tail)) #f)
            (guard (e (#t #t)) (member 2 '(1 . tail)) #f)
            (guard (e (#t #t)) ((member-procedure =) 2 '(1 . tail)) #f)))
(test "member comparator stops at the first matching pair" '((2 . tail) 2)
      (let ((calls 0))
        (let ((result (member 2 '(1 2 . tail)
                        (lambda (a b) (set! calls (+ calls 1)) (= a b)))))
          (list result calls))))
(test "list-ref and list-tail traverse circular lists" '(c #t)
      (let ((cycle (circular-list 'a 'b 'c)))
        (list (list-ref cycle 8) (eq? (list-tail cycle 8) (cddr cycle)))))
(test "indexed list operations preserve dotted tails" '(b tail tail)
      (list (list-ref '(a b . tail) 1)
            (list-tail '(a b . tail) 2)
            (list-tail 'tail 0)))
(test "list-set! follows a bounded circular prefix" '(1 9 3)
      (let ((cycle (circular-list 1 2 3)))
        (list-set! cycle 7 9)
        (take cycle 3)))

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
(test "list= stops before unvisited improper tails" '(#f #f #f)
      (list (list= = '(2) '(1 . tail))
            (list= = '() '(1 . tail))
            (list= = '(2) '(1) 'tail)))
(test "list= still rejects an improper tail reached by comparison" #t
      (guard (e (#t #t)) (list= = '(1) '(1 . tail)) #f))
(test "list= recognizes identical dotted and circular lists" '(#t #t)
      (let ((dotted (cons 1 'tail)) (circular (circular-list 1 2)))
        (list (list= = dotted dotted) (list= = circular circular))))
(test "list= stops when a finite list ends before a circular companion" #f
      (list= = '(1 2) (circular-list 1 2)))
(test "list= avoids comparing a shared tail twice" 1
      (let ((tail (list 2 3)) (count 0))
        (list= (lambda (a b) (set! count (+ count 1)) (= a b))
               (cons 1 tail) (cons 1 tail))
        count))
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
(test "unzip reads only the required tuple fields"
      '((1 6) (2 7) (3 6) (4 7) (5 6))
      (call-with-values
        (lambda ()
          (unzip5 (list '(1 2 3 4 5 . tail) (circular-list 6 7)))) list))
(test "unzip accepts dotted tuple prefixes at every arity" '(1 2 3 4 5)
      (map (lambda (proc)
             (length (call-with-values
                       (lambda () (proc '((1 2 3 4 5 . tail)))) list)))
           (list unzip1 unzip2 unzip3 unzip4 unzip5)))
(test "unzip rejects a missing tuple field" #t
      (guard (e (#t #t)) (unzip2 '((1 . tail))) #f))
(test "concatenate preserves an arbitrary final tail" '(1 2 . tail)
      (concatenate '((1) (2 . tail))))
(test "concatenate accepts a sole atomic tail" 'tail (concatenate '(tail)))
(test "concatenate! accepts dotted prefixes and final tails" '(1 2 . end)
      (concatenate! (list (cons 1 'discarded) (cons 2 'end))))
(test "concatenate shares the final circular tail" #t
      (let ((tail (circular-list 'a 'b)))
        (eq? (cdr (concatenate (list '(prefix) tail))) tail)))
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
(test "gensym is distinct from an interned symbol of the same name" '(#t #f #t)
      (let* ((fresh (gensym))
             (ordinary (string->symbol (symbol->string fresh))))
        (list (symbol? fresh) (eq? fresh ordinary)
              (eq? ordinary (string->symbol (symbol->string fresh))))))
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
(test "map reentry is independent of mutations to an earlier result"
      '((99 2 3) (1 20 3))
      (let ((saved #f) (first #f) (again? #f))
        (let ((result
               (map-in-order
                 (lambda (x)
                   (if (= x 2)
                       (call-with-current-continuation
                         (lambda (k) (set! saved k) x))
                       x))
                 '(1 2 3))))
          (if again?
              (list first result)
              (begin (set! first result) (set-car! first 99)
                     (set! again? #t) (saved 20))))))
(test "filter reentry preserves earlier results" '((1 2 3) (1 3))
      (let ((saved #f) (first #f) (again? #f))
        (let ((result
               (filter (lambda (x)
                         (if (= x 2)
                             (call-with-current-continuation
                               (lambda (k) (set! saved k) #t))
                             #t))
                       '(1 2 3))))
          (if again?
              (list first result)
              (begin (set! first result) (set! again? #t) (saved #f))))))
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
(test "searches accept successful circular inputs" '(2 2 #f 2 (1 3))
      (let ((cycle (circular-list 1 3 2)))
        (list (find even? cycle)
              (any (lambda (x) (and (even? x) x)) cycle)
              (every odd? cycle)
              (list-index even? cycle)
              (take-while odd? cycle))))
(test "searches return the original circular tail" '(#t #t #t #t #t)
      (let* ((cycle (circular-list 1 3 2)) (tail (cddr cycle)))
        (list (eq? (find-tail even? cycle) tail)
              (eq? (drop-while odd? cycle) tail)
              (eq? (memq 2 cycle) tail)
              (eq? (memv 2 cycle) tail)
              (eq? (member 2 cycle) tail))))
(test "span and break split successful circular inputs" '((1 3) #t (1 3) #t)
      (let* ((cycle (circular-list 1 3 2)) (tail (cddr cycle)))
        (call-with-values (lambda () (span odd? cycle))
          (lambda (prefix rest)
            (call-with-values (lambda () (break even? cycle))
              (lambda (prefix2 rest2)
                (list prefix (eq? rest tail) prefix2 (eq? rest2 tail))))))))
(test "any and every allow all-circular companion lists" '(matched #f)
      (list (any (lambda (x y) (and (= x y) 'matched))
                 (circular-list 1 2) (circular-list 3 2 4))
            (every < (circular-list 1 2) (circular-list 3 2 4))))
(test "any and every call the final predicate once" '(last last 4)
      (let ((calls 0))
        (let ((a (any (lambda (x) (set! calls (+ calls 1))
                         (and (= x 2) 'last)) '(1 2)))
              (b (every (lambda (x) (set! calls (+ calls 1))
                           (if (= x 2) 'last 'first)) '(1 2))))
          (list a b calls))))
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
(test "multi-list traversals ignore an unvisited dotted tail"
      '((5) (5) 1 (1 4) (5) (5))
      (list (map + '(1) '(4 . tail))
            (filter-map + '(1) '(4 . tail))
            (count < '(1) '(4 . tail))
            (append-map list '(1) '(4 . tail))
            (pair-fold (lambda (a b acc) (cons (+ (car a) (car b)) acc))
                       '() '(1) '(4 . tail))
            (pair-fold-right (lambda (a b acc) (cons (+ (car a) (car b)) acc))
                             '() '(1) '(4 . tail))))
(test "multi-list traversals skip arguments after an empty list"
      '(() 0 9 ())
      (list (map list '(1) '() 'unused)
            (count < '(1) '() 'unused)
            (pair-fold #f 9 '(1) '() 'unused)
            (zip '(1) '() 'unused)))
(test "folds stop before unvisited dotted tails" '(5 5)
      (list (fold + 0 '(1) '(4 5 . tail))
            (fold-right + 0 '(1) '(4 5 . tail))))
(test "folds validate all current tails at the stopping point" '(#t #t)
      (list (guard (e (#t #t)) (fold + 0 '(1) '(4 . tail)) #f)
            (guard (e (#t #t)) (fold-right + 0 '(1) '(4 . tail)) #f)))
(test "zip accepts a circular companion" '((1 a) (2 b) (3 a))
      (zip '(1 2 3) (circular-list 'a 'b)))
(test "pair-fold captures successors before pair mutation" '(3 2 1)
      (pair-fold (lambda (pair acc) (set-cdr! pair acc) pair)
                 '() (list 1 2 3)))
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
      (let ((path (temporary-file-path)))
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
      (let ((path (temporary-file-path))
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
(test "unread-char follows a re-encoded source prefix" '(#\b #\c)
      (let* ((source (string-copy "abc"))
             (port (open-input-string source)))
        (read-char port)
        (let ((c (read-char port)))
          (string-set! source 0 #\𝄞)
          (unread-char c port)
          (list (read-char port) (read-char port)))))
(test "unread-char follows a shrinking sliced prefix" #\b
      (let* ((source (string-copy "z𝄞bc"))
             (slice (string-slice source 1 4))
             (port (open-input-string slice)))
        (read-char port)
        (let ((c (read-char port)))
          (string-set! source 1 #\a)
          (unread-char c port)
          (read-char port))))
(test "unread-char rejects a source character changed since reading" #t
      (let* ((source (string-copy "ab"))
             (port (open-input-string source))
             (c (read-char port)))
        (string-set! source 0 #\z)
        (guard (e (#t #t)) (unread-char c port) #f)))
(test "unread-char rejects intervening string reads" #t
      (let* ((port (open-input-string "aaa")) (c (read-char port)))
        (read-string 1 port)
        (guard (e (#t #t)) (unread-char c port) #f)))
(test "unread-char validates most recent character" #t
      (guard (e (#t #t))
        (call-with-input-string "ab"
          (lambda (port)
            (read-char port)
            (unread-char #\b port)))
        #f))
(test "call-with-output-string" "test"
      (call-with-output-string (lambda (p) (display "test" p))))
(test "closed output string port retains text and rejects writes" '("λ𝄞" #f #t)
      (let ((p (open-output-string)))
        (display "λ𝄞" p)
        (close-output-port p)
        (close-output-port p)
        (let ((rejected (guard (e (#t #t)) (display "extra" p) #f)))
          (list (get-output-string p) (output-port-open? p) rejected))))
(test "empty closed output string port retains empty text" ""
      (let ((p (open-output-string)))
        (close-output-port p)
        (get-output-string p)))
(test "call-with-port preserves string output for retrieval" "text"
      (let ((p (open-output-string)))
        (call-with-port p (lambda (port) (display "text" port)))
        (get-output-string p)))
(test "call-with-output-string permits closing its port in the callback" "text"
      (call-with-output-string
        (lambda (port) (display "text" port) (close-output-port port))))
(test "polar literals preserve exact zero angles" '(1/2 16 0)
      (list 1/2@0 #x10@0 0@1))
(test "polar conversion preserves huge exact radii" #t
      (let ((radius (expt 10 400)))
        (= (make-polar radius 0) radius)))
(test "polar conversion preserves signed inexact imaginary zero" '(#t #t)
      (let ((imaginary (imag-part (make-polar 2 -0.0))))
        (list (inexact? imaginary) (negative? (/ 1.0 imaginary)))))
(test "polar conversion handles an infinite radius at exact zero angle" #t
      (= (make-polar +inf.0 0) +inf.0))
(test "polar numbers parse consistently through read and string->number" #t
      (let ((text "#x10@1"))
        (= (read (open-input-string text)) (string->number text))))
(test "call-with-input-string" #\a
      (call-with-input-string "abc" (lambda (p) (read-char p))))
(test "with-output-to-string" "test"
      (with-output-to-string (lambda () (display "test"))))
(test "with-input-from-string" #\a
      (with-input-from-string "abc" read-char))
(test "with-output-to-string restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string))
            (escape #f))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (set! escape k)
            (with-output-to-string (lambda () (escape #t)))))
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
                     (with-input-from-string "abc" (lambda () (escape #t)))))
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
      (let ((escaped-port #f)
            (path (temporary-file-path)))
        (call/cc
          (lambda (k)
            (call-with-output-file
              path
              (lambda (p)
                (set! escaped-port p)
                (display "x" p)
                (flush-output-port p)
                (k #t)))))
        (let ((result
               (call-with-input-file
                 path
                 (lambda (p) (read-char p)))))
          (close-output-port escaped-port)
          (delete-file path)
          result)))

(test "with-output-to-file restores on continuation escape" "after"
      (let ((old (current-output-port))
            (p (open-output-string))
            (escaped-port #f)
            (path (temporary-file-path)))
        (set-current-output-port! p)
        (call/cc
          (lambda (k)
            (with-output-to-file
              path
              (lambda ()
                (set! escaped-port (current-output-port))
                (k #t)))))
        (display "after")
        (set-current-output-port! old)
        (close-output-port escaped-port)
        (delete-file path)
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
(test "finite-list consumers accept zero-length dotted lists" '(42 42 ())
      (list (list-copy 42) (take-right 42 0) (drop-right 42 0)))
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
(test "destructive list bounds reject the first missing pair without mutation"
      '((1 2 3) (1 2 3) (1 2 3))
      (map (lambda (proc)
             (let ((lst (list 1 2 3)))
               (guard (e (#t lst)) (proc lst 4) 'no-error)))
           (list take! drop-right! split-at!)))
(test "destructive list bounds reject nonzero counts on empty lists"
      '(#t #t #t)
      (map (lambda (proc)
             (guard (e (#t #t)) (proc '() 1) #f))
           (list take! drop-right! split-at!)))
(test "destructive list prefixes support dotted inputs"
      '((1 2) (1 2) ((1 2) tail))
      (list (take! (cons 1 (cons 2 'tail)) 2)
            (drop-right! (cons 1 (cons 2 'tail)) 0)
            (call-with-values
              (lambda () (split-at! (cons 1 (cons 2 'tail)) 2)) list)))
(test "destructive zero-length prefixes accept atoms" '(() () (() tail))
      (list (take! 'tail 0) (drop-right! 'tail 0)
            (call-with-values (lambda () (split-at! 'tail 0)) list)))
(test "searches stop before an unvisited dotted tail" '(2 1 (1) (2 . tail))
      (list (find even? '(1 2 . tail))
            (list-index even? '(1 2 . tail))
            (take-while odd? '(1 2 . tail))
            (drop-while odd? '(1 2 . tail))))
(test "partition! classifies each element once" '(((1 3) (2)) 3)
      (let ((calls 0))
        (let ((result
               (call-with-values
                 (lambda ()
                   (partition! (lambda (x)
                                 (set! calls (+ calls 1)) (odd? calls))
                               (list 1 2 3))) list)))
          (list result calls))))
(test "span classifies each visited element once" '(((a b) (c d)) 3)
      (let ((calls 0))
        (let ((result
               (call-with-values
                 (lambda ()
                   (span (lambda (x) (set! calls (+ calls 1)) (< calls 3))
                         '(a b c d))) list)))
          (list result calls))))
(test "break classifies each visited element once" '(((a b) (c d)) 3)
      (let ((calls 0))
        (let ((result
               (call-with-values
                 (lambda ()
                   (break (lambda (x) (set! calls (+ calls 1)) (>= calls 3))
                          '(a b c d))) list)))
          (list result calls))))
(test "partition! preserves dotted tails in both results"
      '((2 . tail) (1 3 . tail))
      (call-with-values
        (lambda () (partition! even? (cons 1 (cons 2 (cons 3 'tail))))) list))
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
(test "last accepts a dotted list" 1 (last (cons 1 2)))
(test "last rejects empty, atomic, and circular inputs" '(#t #t #t)
      (list
       (guard (e (#t #t)) (last '()) #f)
       (guard (e (#t #t)) (last 42) #f)
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
(test "complex magnitude" 5 (magnitude (make-rectangular 3 4)))
(test "complex magnitude is exact when rational, like MIT" '(#t 5/4 #f 5.0 1.4142135623730951)
      (list (exact? (magnitude 3+4i)) (magnitude 3/4+1i) (exact? (magnitude 1+i))
            (magnitude 3.0+4i) (magnitude 1+i)))
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

(test "hash tables recognize equal cycles of different lengths"
      '(#t #t first 1 second #f)
      (let ((table (make-hash-table))
            (one (circular-list 'x))
            (two (circular-list 'x 'x)))
        (hash-table-set! table one 'first)
        (let ((before (hash-table-ref table two (lambda () 'missing))))
          (hash-table-set! table two 'second)
          (let ((size (hash-table-size table))
                (after (hash-table-ref table one)))
            (hash-table-delete! table two)
            (list (equal? one two) (= (hash one) (hash two))
                  before size after (hash-table-exists? table one))))))
(test "hash tables recognize equivalent vector cycles" '(#t #t found)
      (let ((table (make-hash-table))
            (one (make-vector 1))
            (two (make-vector 1))
            (other (make-vector 1)))
        (vector-set! one 0 one)
        (vector-set! two 0 other)
        (vector-set! other 0 two)
        (hash-table-set! table one 'found)
        (list (equal? one two) (= (hash one) (hash two))
              (hash-table-ref table two (lambda () 'missing)))))
(test "hashing shared acyclic graphs has bounded work" #t
      (let loop ((depth 40) (left 'leaf) (right 'leaf))
        (if (= depth 0)
            (= (hash left) (hash right))
            (loop (- depth 1) (vector left left)
                  (vector right right)))))

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
; Bytevector ports are records, not vectors: no vector operation can reach
; their state, and port? and vector? are disjoint.
(test "bytevector ports are not vectors"
      '(#f #f error error #t #t #t #t #t #t #f #f)
      (let ((in (open-input-bytevector (bytevector 1)))
            (out (open-output-bytevector)))
        (list (vector? in)
              (vector? out)
              (guard (e (#t 'error)) (vector-set! in 2 99))
              (guard (e (#t 'error)) (vector-ref out 1))
              (port? in)
              (port? out)
              (input-port? in)
              (output-port? out)
              (binary-port? in)
              (binary-port? out)
              (textual-port? in)
              (textual-port? out))))
(test "bytevector ports still work after a vector-set! attempt" '(1 eof #u8(65))
      (let ((in (open-input-bytevector (bytevector 1)))
            (out (open-output-bytevector)))
        (guard (e (#t #f)) (vector-set! in 2 99))
        (guard (e (#t #f)) (vector-set! out 1 7))
        (write-u8 65 out)
        (let* ((first (read-u8 in))
               (second (read-u8 in)))
          (list first
                (if (eof-object? second) 'eof second)
                (get-output-bytevector out)))))
(test "bytevector ports print like MIT's" '(#t #t)
      (list (string-prefix? "#[binary-input-port "
                            (write-to-string (open-input-bytevector (bytevector))))
            (string-prefix? "#[binary-output-port "
                            (write-to-string (open-output-bytevector)))))
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

; File/OS helpers. Each run owns its files so VM and interpreter suites
; can execute concurrently without overwriting or deleting each other's data.
(define compat-file (temporary-file-path))
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
(define compat-transcript-file (temporary-file-path))
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
(define compat-text-file (temporary-file-path))
(define compat-text-out (open-output-file compat-text-file))
(test "text output port predicate" #t (textual-port? compat-text-out))
(test "text output not binary" #f (binary-port? compat-text-out))
(close-output-port compat-text-out)
(delete-file compat-text-file)
(test "current-directory returns string" #t (string? (current-directory)))
(test "directory-files returns list" #t (list? (directory-files ".")))
(define compat-dir (temporary-file-path))
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
; Char-sets are records, but equal? compares them by contents, as MIT does:
; finite sets by their (sorted, coalesced) ranges, predicate-based sets by
; identity. member, assoc and equal hash tables follow.
(test "equal? on character sets" '(#t #t #t #f #f #t #t #f)
      (let ((inverted (char-set-invert (char-set #\a))))
        (list (equal? (char-set #\a #\b) (char-set #\b #\a))
              (equal? (char-set "abc") (char-set #\c (cons 97 99)))
              (equal? (char-set-union (char-set #\a) (char-set #\b))
                      (char-set #\a #\b))
              (equal? (char-set #\a #\b) (char-set #\a #\c))
              (equal? (char-set #\a) (char-set))
              (equal? inverted inverted)
              (equal? (list (char-set #\x)) (list (char-set #\x)))
              (equal? inverted (char-set-invert (char-set #\a))))))
(test "member, assoc and equal hash tables find equal character sets"
      '((#t) found ab none)
      (let ((table (make-equal-hash-table)))
        (hash-table-set! table (char-set #\a #\b) 'ab)
        (list (map char-set?
                   (member (char-set #\y #\x) (list 1 (char-set #\x #\y))))
              (cdr (assoc (char-set "xy")
                          (list (cons (char-set #\y #\x) 'found))))
              (hash-table-ref/default table (char-set #\b #\a) 'none)
              (hash-table-ref/default table (char-set #\a) 'none))))
(test "other records are equal? only to themselves" '(#f #t)
      (let ()
        (define-record-type cell (make-cell v) cell? (v cell-v))
        (let ((c (make-cell 1)))
          (list (equal? (make-cell 1) (make-cell 1)) (equal? c c)))))
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
(test "character digit conversions" '(#t #t #f #\e #\9)
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
(define include-test-file (temporary-file-path))
(define include-test-out (open-output-file include-test-file))
(write-string "(define included-value 77)" include-test-out)
(close-output-port include-test-out)
(include include-test-file)
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
(test "record constructor aliases retain their record type" '(#t 2)
      (let ()
        (define-record-type thing (make-thing x) thing? (x thing-x))
        (let ((constructor make-thing))
          (set! make-thing (lambda (x) x))
          (let ((obj (constructor 2)))
            (list (thing? obj) (thing-x obj))))))
(test "record field procedures retain their predicate" '(1 2)
      (let ()
        (define-record-type thing (make-thing x) thing? (x thing-x set-thing-x!))
        (let ((obj (make-thing 1)))
          (set! thing? (lambda (x) #f))
          (let ((before (thing-x obj)))
            (set-thing-x! obj 2)
            (list before (thing-x obj))))))
(test "record predicate rebinding cannot bypass field type checks" '(#t #t)
      (let ()
        (define-record-type thing (make-thing x) thing? (x thing-x set-thing-x!))
        (set! thing? (lambda (x) #t))
        (list (guard (e (#t #t)) (thing-x (vector 'wrong 7)) #f)
              (guard (e (#t #t)) (set-thing-x! (vector 'wrong 7) 2) #f))))
(test "record fields do not capture the constructor type tag" '(#t 1)
      (let ()
        (define-record-type thing
          (make-thing make-thing) thing? (make-thing thing-x))
        (let ((obj (make-thing 1)))
          (list (thing? obj) (thing-x obj)))))
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

; The names are checked before anything is defined, so a bad one is
; define-record-type's error, not define's.
(test "define-record-type reports a bad name itself"
      '("define-record-type: type name must be a symbol \"p\""
        "define-record-type: predicate name must be a symbol 5")
      (list (guard (e (#t (condition/report-string e)))
              (eval '(define-record-type "p" (mk a) p? (a pa))
                    (interaction-environment)))
            (guard (e (#t (condition/report-string e)))
              (let ()
                (define-record-type q #f 5 (a qa))
                #f))))
(test "define-record-type reports a bad type name in a body too"
      "define-record-type: type name must be a symbol 7"
      (guard (e (#t (condition/report-string e)))
        (let ()
          (define-record-type 7 (mk a) p? (a pa))
          #f)))

(section "Opaque Records")

; Records are their own type (R7RS 3.2): no other predicate accepts one, and
; no vector or list operation reaches inside.
(test "records are disjoint from other types"
      '(#f #f #f #f #f #f #f #f #f #f #f #t #t)
      (let ((r (make-record-a 1)))
        (list (vector? r) (procedure? r) (pair? r) (null? r) (string? r)
              (symbol? r) (number? r) (char? r) (boolean? r) (bytevector? r)
              (port? r) (record? r) (record-a? r))))
(test "vector and list operations reject records"
      '(error error error error error error)
      (let ((r (make-reordered 1 2)))
        ; Through lambdas, so the VM's inline vector opcodes run too.
        (map (lambda (op) (guard (e (#t 'error)) (op) 'accepted))
             (list (lambda () (vector-ref r 0))
                   (lambda () (vector-set! r 0 'forged))
                   (lambda () (vector-length r))
                   (lambda () (vector->list r))
                   (lambda () (length r))
                   (lambda () (car r))))))
(test "record fields survive rejected vector writes" '(1 2)
      (let ((r (make-reordered 2 1)))
        (guard (e (#t #f)) (vector-set! r 1 'forged))
        (list (reordered-x r) (reordered-y r))))
(test "vectors cannot forge records" '(#f #f #f)
      (list (record-a? (vector make-record-a 1))
            (record-a? (vector record-collision 1))
            (reordered-record? (list->vector (list reordered-record 1 2)))))
(test "the type name is bound to the record type" '(#t #t #f #t)
      (list (record-type? reordered-record)
            (eq? (record-type-descriptor reordered-record-instance)
                 reordered-record)
            (record? reordered-record)
            ; The second record-collision definition rebound the name.
            (eq? (record-type-descriptor record-b-instance) record-collision)))
(test "record-type-name keeps brackets; field names in order"
      '("reordered-record" "<bracketed>" (x y) (a))
      (let ()
        (define-record-type <bracketed> (make-bracketed a) bracketed?
          (a bracketed-a))
        (list (record-type-name reordered-record)
              (record-type-name <bracketed>)
              (record-type-field-names reordered-record)
              (record-type-field-names <bracketed>))))
(test "records print as #[name N], without brackets" '(#t #t #t #t #t)
      (let ()
        (define-record-type <bracketed> (make-bracketed a) bracketed?
          (a bracketed-a))
        (let* ((r (make-bracketed 1))
               (text (write-to-string r)))
          (list (string-prefix? "#[bracketed " text)
                (string-suffix? "]" text)
                ; The number is the object's: stable, and unique to it.
                (string=? text (write-to-string r))
                (not (string=? text (write-to-string (make-bracketed 1))))
                (string-prefix? "#[record-type "
                                (write-to-string <bracketed>))))))
(test "record printing survives a collection" #t
      (let* ((r (make-record-a 5))
             (before (write-to-string r)))
        (gc-flip)
        (string=? before (write-to-string r))))
(test "records evaluate to themselves" '(#t #t)
      (let ((r (make-record-a 1)))
        (list (eq? r (eval r (interaction-environment)))
              (eq? r (car (eval (list 'list r) (interaction-environment)))))))
(test "records compare by identity" '(#f #t #t #t)
      (let ((r (make-reordered 1 2)))
        (list (equal? (make-reordered 1 2) (make-reordered 1 2))
              (equal? r r) (eqv? r r) (eq? r r))))
(test "records as hash table keys across collections" '(1 2 none 1 2 none none)
      (let ((a (make-reordered 1 2))
            (b (make-reordered 1 2))
            (eq-table (make-strong-eqv-hash-table))
            (equal-table (make-equal-hash-table)))
        (hash-table-set! eq-table a 1)
        (hash-table-set! eq-table b 2)
        (hash-table-set! equal-table a 1)
        (hash-table-set! equal-table b 2)
        (gc-flip)
        (list (hash-table-ref/default eq-table a 'none)
              (hash-table-ref/default eq-table b 'none)
              (hash-table-ref/default eq-table (make-reordered 1 2) 'none)
              (hash-table-ref/default equal-table a 'none)
              (hash-table-ref/default equal-table b 'none)
              (hash-table-ref/default equal-table (make-reordered 1 2) 'none)
              (hash-table-ref/default equal-table (vector reordered-record 1 2)
                                      'none))))
(test "a record's hash survives a collection" #t
      (let* ((r (make-reordered 1 2))
             (before (hash r)))
        (gc-flip)
        (= before (hash r))))
(test "record accessor error names the accessor and type"
      '("record-a-x: not a record-collision record"
        "set-record-a-x!: not a record-collision record"
        "bracketed-a: not a bracketed record")
      (let ()
        (define-record-type <bracketed> (make-bracketed a) bracketed?
          (a bracketed-a))
        (map (lambda (thunk)
               (guard (e ((error-object? e) (error-object-message e)))
                 (thunk)))
             (list (lambda () (record-a-x 5))
                   (lambda () (set-record-a-x! record-b-instance 1))
                   (lambda () (bracketed-a (make-record-a 1)))))))
(test "record field and argument names cannot capture the expansion"
      '(1 2 3 4 50 #t)
      (let ()
        (define-record-type node (make-node rtd layout index obj value) node?
          (rtd node-rtd) (layout node-layout) (index node-index)
          (obj node-obj) (value node-value set-node-value!))
        (let ((n (make-node 1 2 3 4 5)))
          (set-node-value! n 50)
          (list (node-rtd n) (node-layout n) (node-index n) (node-obj n)
                (node-value n) (node? n)))))
(test "a field named like the type" '(7 #t)
      (let ()
        (define-record-type point (make-point point) point? (point point-x))
        (list (point-x (make-point 7)) (record-type? point))))
(test "rebinding the type name does not affect the record procedures"
      '(#t 1)
      (let ()
        (define-record-type thing (make-thing x) thing? (x thing-x))
        (set! thing 'gone)
        (let ((obj (make-thing 1)))
          (list (thing? obj) (thing-x obj)))))
(test "define-record-type with a bare constructor name" '(1 2 #t)
      (let ()
        (define-record-type point make-point point? (x point-x) (y point-y))
        (let ((p (make-point 1 2)))
          (list (point-x p) (point-y p) (point? p)))))
(test "define-record-type with no constructor" '(#f #t)
      (let ()
        (define-record-type point #f point? (x point-x))
        (list (point? 5) (record-type? point))))
(test "define-record-type in an imported environment" '(#t 4 #f)
      (eval '(let ()
               (define-record-type pt (mk x) pt? (x pt-x))
               (list (pt? (mk 3)) (pt-x (mk 4)) (vector? (mk 5))))
            (environment '(scheme base))))
(test "define-record-type with many fields in a body" '(0 39 #t)
      (let ()
        (define-record-type wide
          (make-wide f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16
                     f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30
                     f31 f32 f33 f34 f35 f36 f37 f38 f39)
          wide?
          (f0 wide-f0) (f1 wide-f1) (f2 wide-f2) (f3 wide-f3) (f4 wide-f4)
          (f5 wide-f5) (f6 wide-f6) (f7 wide-f7) (f8 wide-f8) (f9 wide-f9)
          (f10 wide-f10) (f11 wide-f11) (f12 wide-f12) (f13 wide-f13)
          (f14 wide-f14) (f15 wide-f15) (f16 wide-f16) (f17 wide-f17)
          (f18 wide-f18) (f19 wide-f19) (f20 wide-f20) (f21 wide-f21)
          (f22 wide-f22) (f23 wide-f23) (f24 wide-f24) (f25 wide-f25)
          (f26 wide-f26) (f27 wide-f27) (f28 wide-f28) (f29 wide-f29)
          (f30 wide-f30) (f31 wide-f31) (f32 wide-f32) (f33 wide-f33)
          (f34 wide-f34) (f35 wide-f35) (f36 wide-f36) (f37 wide-f37)
          (f38 wide-f38) (f39 wide-f39 set-wide-f39!))
        (let ((w (make-wide 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
                            20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
                            37 38 39)))
          (list (wide-f0 w) (wide-f39 w) (wide? w)))))

(section "MIT Record Procedures")

(test "record? matches MIT"
      '(#t #f #f #f #f #f #t #t #t #t #f)
      (list (record? (make-record-a 1))
            (record? record-collision)
            (record? 5)
            (record? (vector 1))
            (record? (delay 1))
            (record? (make-promise 1))
            (record? (char-set #\a))
            (record? (open-input-bytevector (bytevector 1)))
            (record? (open-output-bytevector))
            (record? (guard (e (#t e)) (error "boom")))
            (record? car)))
(test "make-record-type and the procedural API" '("q" #t 1 5 #f #t (a b))
      (let* ((q (make-record-type 'q '(a b)))
             (make-q (record-constructor q))
             (q? (record-predicate q))
             (q-a (record-accessor q 'a))
             (set-q-b! (record-modifier q 'b))
             (qq (make-q 1 2)))
        (set-q-b! qq 5)
        (list (record-type-name q) (q? qq) (q-a qq) ((record-accessor q 'b) qq)
              (q? (make-record-a 1)) (record? qq) (record-type-field-names q))))
(test "record-constructor with a field list leaves the rest #f" '(#f 9 (1 #f 3))
      (let* ((q (make-record-type "q" '(a b c)))
             (make-q2 (record-constructor q '(b)))
             (make-q3 (record-constructor q '(c a)))
             (r (make-q2 9))
             (s (make-q3 3 1)))
        (list ((record-accessor q 'a) r)
              ((record-accessor q 'b) r)
              (map (lambda (f) ((record-accessor q f) s)) '(a b c)))))
(test "record-constructor of five or more fields" '(1 5)
      (let* ((q (make-record-type 'q '(a b c d e)))
             (r ((record-constructor q) 1 2 3 4 5)))
        (list ((record-accessor q 'a) r) ((record-accessor q 'e) r))))
(test "define-record-type types work with the procedural API" '(7 8 #t)
      (let ((r ((record-constructor reordered-record '(y x)) 8 7)))
        (list (reordered-x r) ((record-accessor reordered-record 'y) r)
              ((record-predicate reordered-record) r))))
(test "the procedural API rejects bad arguments"
      '(error error error error error error error error)
      (let ((q (make-record-type 'q '(a b))))
        (map (lambda (thunk) (guard (e (#t 'error)) (thunk) 'accepted))
             (list (lambda () (record-accessor q 'c))
                   (lambda () ((record-accessor q 'a) 1))
                   (lambda () ((record-accessor q 'a) (make-record-a 1)))
                   (lambda () ((record-constructor q) 1))
                   (lambda () (record-constructor q '(a a)))
                   (lambda () (make-record-type 'bad '(x x)))
                   (lambda () (record-type-descriptor 5))
                   (lambda () (record-type-descriptor (delay 1)))))))
(test "runtime record types cannot be built or modified" '(error error #t)
      (let ((char-set-type (record-type-descriptor (char-set #\a))))
        (list (guard (e (#t 'error)) (record-constructor char-set-type) #f)
              (guard (e (#t 'error))
                (record-modifier char-set-type 'kind) #f)
              (record-type? char-set-type))))

(section "Opaque Runtime Objects")

(test "error objects are conditions, not vectors" '(#f #t #t "boom" (1 2))
      (let ((e (guard (e (#t e)) (error "boom" 1 2))))
        (list (vector? e) (record? e) (error-object? e)
              (error-object-message e) (error-object-irritants e))))
(test "error objects print as MIT conditions" '(#t #t #t)
      (let ((e (guard (e (#t e)) (error "boom")))
            (display-to-string
             (lambda (x)
               (call-with-output-string (lambda (port) (display x port))))))
        (list (string-prefix? "#[condition " (write-to-string e))
              (string-suffix? " \"simple-error\"]" (write-to-string e))
              (string-suffix? " simple-error]" (display-to-string e)))))
(test "vectors cannot forge error objects" '(#f #f)
      (list (error-object? (vector 'error-object 'error "boom" '()))
            (file-error? (vector (list 'error-object) 'file "x" '()))))
(test "condition/report-string" '("boom 1 \"two\" three" "car: not a pair")
      (list (guard (e (#t (condition/report-string e)))
              (error "boom" 1 "two" 'three))
            (guard (e (#t (condition/report-string e))) (car (list-tail '(1) 1)))))
(test "the default handler reports a condition's message and irritants"
      "Unhandled exception: boom 1 \"two\"\n"
      (let ((e (guard (e (#t e)) (error "boom" 1 "two")))
            (saved *wind-stack*))
        (let ((text (call-with-output-string
                     (lambda (port)
                       (let ((old (current-output-port)))
                         (set-current-output-port! port)
                         (*default-exception-handler* e)
                         (set-current-output-port! old))))))
          ; The handler resets the wind stack, as it does when it ends a
          ; computation; this one continues.
          (set! *wind-stack* saved)
          text)))
; make-error-object takes any irritants object, as R7RS raise takes any
; object; a non-list one is written whole, not walked as a list.
(test "condition/report-string with irritants that are not a list"
      '("x 5" "x (1 . 2)" "x")
      (list (condition/report-string (make-error-object 'error "x" 5))
            (condition/report-string (make-error-object 'error "x" '(1 . 2)))
            (condition/report-string (make-error-object 'error "x" '()))))
(test "the default handler reports non-list irritants"
      "Unhandled exception: x 5\n"
      (let ((e (guard (e (#t e)) (raise (make-error-object 'error "x" 5))))
            (saved *wind-stack*))
        (let ((text (call-with-output-string
                     (lambda (port)
                       (let ((old (current-output-port)))
                         (set-current-output-port! port)
                         (*default-exception-handler* e)
                         (set-current-output-port! old))))))
          (set! *wind-stack* saved)
          text)))
(test "promises are opaque" '(#f #f #t #f)
      (list (vector? (delay 1)) (record? (delay 1)) (promise? (delay 1))
            (promise? (vector (list 'promise) (cons #t 1)))))
(test "promises print their state like MIT's"
      '(#t #t #t #t)
      (let ((p (delay (+ 1 2))))
        (let ((before (write-to-string p)))
          (force p)
          (list (string-prefix? "#[promise " before)
                (string-suffix? " (unevaluated)]" before)
                (string-suffix? " (evaluated) 3]" (write-to-string p))
                (string-suffix? " (evaluated) \"s\"]"
                                (write-to-string (make-promise "s")))))))
(test "a promise whose value contains it prints" #t
      (let* ((cell (list 1))
             (p (make-promise cell)))
        (set-car! cell p)
        (string? (write-to-string p))))
(test "char-sets and their cursors are opaque" '(#f #t #t #f #f #t)
      (let* ((set (char-set #\a #\b))
             (cursor (char-set-cursor set)))
        (list (vector? set) (record? set) (char-set? set)
              (vector? cursor)
              (char-set? (vector 'char-set 'ranges '()))
              (string-prefix? "#[char-set " (write-to-string set)))))
(test "char-set cursors walk to the end" '((#\a #\b) #t)
      (let ((set (char-set #\a #\b)))
        (let loop ((cursor (char-set-cursor set)) (chars '()))
          (if (end-of-char-set? cursor)
              (list (reverse chars) (end-of-char-set? cursor))
              (loop (char-set-cursor-next set cursor)
                    (cons (char-set-ref set cursor) chars))))))
(test "char-set operations reject non-cursors" '(error error)
      (let ((set (char-set #\a)))
        (list (guard (e (#t 'error)) (char-set-ref set (vector 'char-set-cursor set 97)))
              (guard (e (#t 'error)) (end-of-char-set? 5) (char-set-ref set 5)))))

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
(test "reader case directive persists per string port" '("FOO" "bar" "BAZ" "quux")
      (let ((p (open-input-string "#!no-fold-case FOO BAZ #!fold-case QUUX"))
            (q (open-input-string "BAR")))
        (let* ((a (read p)) (b (read q)) (c (read p)) (d (read p)))
          (map symbol->string (list a b c d)))))
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
(test "string-joiner" "<a>|<b>" ((string-joiner 'infix "|" 'prefix "<" 'suffix ">") "a" "b"))
(test "string-joiner validates keyword strings" #t
      (guard (e (#t #t)) (string-joiner 'infix 42) #f))
(test "string-joiner*" "<a>|<b>" ((string-joiner* 'infix "|" 'prefix "<" 'suffix ">") '("a" "b")))
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
(test "deep string-slices retain shared Unicode storage"
      '("Xd" "aéXYef" "aéλλef" "λλ" "éλλe")
      (let* ((original (string-copy "aé𝄞def"))
             (sibling (string-slice original 1 5))
             (nested (let loop ((i 1100) (slice sibling))
                       (if (= i 0)
                           (string-slice slice 1 3)
                           (loop (- i 1) (string-slice slice))))))
        (string-set! original 2 #\X)
        (let ((after-parent-write (string-copy nested)))
          (string-set! nested 1 #\Y)
          (let ((after-slice-write (string-copy original)))
            (string-fill! nested #\λ)
            (list after-parent-write after-slice-write
                  original nested sibling)))))
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
;;; Stdlib regressions
;;; ============================================================================

(section "Stdlib regressions")

;; guard follows the R7RS 7.3 reference expansion.
(test "guard supports => clauses" 42
      (guard (e ((assq 'a e) => cdr) ((assq 'b e)))
        (raise (list (cons 'a 42)))))
(test "guard test-only clause returns the test value" '(b . 23)
      (guard (e ((assq 'a e) => cdr) ((assq 'b e)))
        (raise (list (cons 'b 23)))))
(test "guard re-raises with raise-continuable when no clause matches" 43
      (with-exception-handler
        (lambda (e) 42)
        (lambda ()
          (+ 1 (guard (e (#f 'no)) (raise-continuable 'oops))))))
(test "guard clauses run after unwinding to the guard" '(in out handler)
      (let ((log '()))
        (guard (e (#t (set! log (cons 'handler log))))
          (dynamic-wind
            (lambda () (set! log (cons 'in log)))
            (lambda () (raise 'x))
            (lambda () (set! log (cons 'out log)))))
        (reverse log)))
(test "guard clauses see the guard's parameterization" 1
      (let ((p (make-parameter 1)))
        (guard (e (#t (p)))
          (parameterize ((p 2)) (raise 'x)))))
(test "guard passes multiple body values through" '(1 2)
      (call-with-values (lambda () (guard (e (#t 0)) (values 1 2))) list))
(test "unmatched guard re-raise reaches the enclosing guard" '(outer sym)
      (guard (e ((symbol? e) (list 'outer e)))
        (guard (e ((string? e) 'inner))
          (raise 'sym))))

;; Bytevector output ports validate their state in O(1); this used to walk
;; every chunk on each write, so 2500 write-u8 calls took about 9 seconds.
(test "bytevector output port handles many single-byte writes" '(10000 255 15)
      (let ((port (open-output-bytevector)))
        (do ((i 0 (+ i 1))) ((= i 10000))
          (write-u8 (modulo i 256) port))
        (let ((bv (get-output-bytevector port)))
          (list (bytevector-length bv)
                (bytevector-u8-ref bv 255)
                (bytevector-u8-ref bv 9999)))))

;; Set operations on finite character sets stay range-based instead of
;; becoming predicates that every enumeration scans over all of Unicode.
(test "char-set-union of finite sets is finite" '(2 ((97 . 99)))
      (let ((set (char-set-union (char-set #\a) (char-set #\b #\a))))
        (list (char-set-size set) (char-set->code-points set))))
(test "char-set-intersection with a finite argument" '((15 . 20))
      (char-set->code-points
       (char-set-intersection (char-set (cons 10 20)) (char-set (cons 15 30) 40))))
(test "char-set-intersection mixing finite and predicate sets" '(#\a #\z)
      (char-set->list
       (char-set-intersection (string->char-set "abcxyz1")
                              char-set:alphabetic
                              (char-set #\a #\z #\q #\1))))
(test "char-set-difference of a finite set" '((10 . 12) (13 . 15) (17 . 20))
      (char-set->code-points
       (char-set-difference (char-set (cons 10 20)) (char-set 12 (cons 15 17)))))
(test "char-set-difference of a finite set and a predicate set" '(#\a #\b)
      (char-set->list
       (char-set-difference (string->char-set "a1b2") char-set:numeric)))
(test "char-set sizes of ASCII splits" '(76 52)
      (list (char-set-size (char-set-difference char-set:ascii char-set:alphabetic))
            (char-set-size (char-set-intersection char-set:ascii char-set:alphabetic))))
(test "char-set with a base set stays finite" '(#\a #\x #\y)
      (char-set->list (list->char-set (list #\x #\y) (char-set #\a))))

;; append! remembers the last pair instead of rescanning the result.
(test "append-map! over many singleton lists" 20000
      (length (append-map! list (iota 20000))))
(test "append! edge cases" '((1 2 3 . 4) () 5 (1 2) (1 3) (1))
      (list (append! (list 1 2) '() (list 3) 4)
            (append!)
            (append! '() 5)
            (append! (list 1) 5 (list 2))
            (append! (cons 1 2) (list 3))
            (append! (list 1) '() '())))

;; digit-value agrees with char-numeric? (Unicode Nd digits).
(test "digit-value on Unicode digit blocks" '(#f 0 9 1 1 3 9 5 #f)
      (map digit-value
           (list (integer->char #x1A10) (integer->char #x1D7D8)
                 (integer->char #x1D7FF) (integer->char #x0DE7)
                 (integer->char #x1091) (integer->char #x104A3)
                 (integer->char #x16B59) #\5 #\a)))
(test "digit-value is consistent with char-numeric? through U+1FFFF" 0
      (let loop ((n 0) (bad 0))
        (if (> n #x1FFFF)
            bad
            (loop (+ n 1)
                  (if (and (>= n #xD800) (<= n #xDFFF))
                      bad
                      (let* ((c (integer->char n)) (dv (digit-value c)))
                        (if (eq? (char-numeric? c) (and dv (<= 0 dv 9) #t))
                            bad
                            (+ bad 1))))))))
(test "char->digit on a double-struck digit" 0
      (char->digit (integer->char #x1D7D8) 10))
(test "digit->char uses lower-case letters like MIT" '(#\0 #\9 #\a #\z)
      (map (lambda (d) (digit->char d 36)) '(0 9 10 35)))

;; MIT joiner semantics: prefix and suffix wrap each joined element.
(test "string-joiner applies the suffix to each element" "a., b."
      ((string-joiner 'infix ", " 'suffix ".") "a" "b"))
(test "string-joiner* wraps each element" "[a] [b] [c]"
      ((string-joiner* 'prefix "[" 'suffix "]" 'infix " ") '("a" "b" "c")))
(test "string-joiner* with no strings" ""
      ((string-joiner* 'prefix "[" 'suffix "]") '()))

;; list-index accepts several lists (SRFI-1).
(test "list-index over several lists" '(1 #f 3)
      (list (list-index = '(1 2 3) '(3 2 1))
            (list-index < '(3 2 1) '(1 1))
            (list-index even? '(1 3 5 6))))

;; SRFI-1 fixes the argument order of the element-equality procedure.
(define (%calls-in-order? run)
  ; RUN receives a recording equality on integers; every call must be
  ; (= x y) with x from the earlier list or the earlier list position.
  (let ((calls '()))
    (run (lambda (a b) (set! calls (cons (list a b) calls)) (= a b)))
    (and (pair? calls)
         (every (lambda (call) (< (car call) (cadr call))) calls))))
(test "lset and delete-duplicates equality argument order" '(#t #t #t #t #t #t)
      (list (%calls-in-order? (lambda (= ) (lset-intersection = '(1 2) '(3 4))))
            (%calls-in-order? (lambda (= ) (lset-difference = '(1 2) '(3 4))))
            (%calls-in-order? (lambda (= ) (lset<= = '(1) '(2 3))))
            (%calls-in-order? (lambda (= ) (lset= = '(1) '(2))))
            (%calls-in-order? (lambda (= ) (lset-adjoin = '(1 2) 3)))
            (%calls-in-order? (lambda (= ) (delete-duplicates '(1 2 3) =)))))
(test "delete-duplicates with an asymmetric predicate" '(1)
      (delete-duplicates '(1 2 3 4) <))
(test "lset results are unchanged" '((a e) (b c d) #t #t)
      (list (lset-intersection eq? '(a b c d e) '(a e i o u))
            (lset-difference eq? '(a b c d e) '(a e i o u))
            (lset<= eq? '(a) '(a b a) '(a b c c))
            (lset= eq? '(b e a) '(a e b) '(e e b a))))

;; string->vector and vector->string are linear and keep ranges.
(test "string->vector and vector->string ranges" '(#(#\b #\c) "bc" "zb")
      (list (string->vector "abcde" 1 3)
            (vector->string (vector #\a #\b #\c) 1)
            (let ((s (vector->string (vector #\a #\b))))
              (string-set! s 0 #\z)
              s)))
(test "string->vector and vector->string on long inputs" '(40000 40000)
      (list (vector-length (string->vector (make-string 40000 #\a)))
            (string-length (vector->string (make-vector 40000 #\a)))))

;; Single-list right folds run in constant stack depth (the recursive
;; versions overflowed the VM stack near 350k elements).
(test "fold-right over a long list" 400000
      (length (fold-right cons '() (iota 400000))))
(test "reduce-right over a long list" 79999800000
      (reduce-right + 0 (iota 400000)))
(test "pair-fold-right over a long list" 400000
      (length (pair-fold-right (lambda (p acc) (cons (car p) acc)) '()
                               (iota 400000))))
(test "single-list right folds keep their call order" '((1 (2 (3 z))) (1 (2 3)) ((a b c) (b c) (c) . z))
      (list (fold-right list 'z '(1 2 3))
            (reduce-right list 0 '(1 2 3))
            (pair-fold-right cons 'z '(a b c))))
(test "single-list folds reject improper lists" '(#t #t #t #t)
      (list (guard (e (#t #t)) (fold-right cons '() '(1 2 . 3)) #f)
            (guard (e (#t #t)) (fold cons '() '(1 2 . 3)) #f)
            (guard (e (#t #t)) (count even? '(2 . 3)) #f)
            (guard (e (#t #t)) (fold-right cons '() (circular-list 1)) #f)))
(test "single-list fold and count fast paths" '((3 2 1) 2 0)
      (list (fold cons '() '(1 2 3))
            (count even? '(1 2 3 4))
            (count even? '())))

;; hash-table-ref accepts an optional success procedure (SRFI-125, MIT).
(test "hash-table-ref fail and succeed procedures" '(10 fail 1 #t)
      (let ((table (make-equal-hash-table)))
        (hash-table-set! table 'a 1)
        (list (hash-table-ref table 'a (lambda () 'fail) (lambda (v) (* v 10)))
              (hash-table-ref table 'b (lambda () 'fail) (lambda (v) (* v 10)))
              (hash-table-ref table 'a)
              (guard (e (#t #t)) (hash-table-ref table 'b) #f))))

;; Port redirections restore the port that was current when the extent was
;; (re)entered, not the one current at the original call.
(test "with-output-to-string restores the port current at re-entry" #t
      (let ((a (open-output-string))
            (b (open-output-string))
            (orig (current-output-port))
            (k #f)
            (entries 0))
        (set-current-output-port! a)
        (with-output-to-string
          (lambda ()
            (call-with-current-continuation (lambda (c) (set! k c)))
            (set! entries (+ entries 1))))
        (if (= entries 1)
            (begin
              (set-current-output-port! b)
              (k #f)))
        (let ((restored-b? (eq? (current-output-port) b)))
          (set-current-output-port! orig)
          restored-b?)))
(test "with-output-to-file restores the port current at re-entry" #t
      (let ((path (temporary-file-path))
            (b (open-output-string))
            (orig (current-output-port))
            (k #f)
            (entries 0))
        (with-output-to-file path
          (lambda ()
            (call-with-current-continuation (lambda (c) (set! k c)))
            (set! entries (+ entries 1))))
        (if (= entries 1)
            (begin
              (set-current-output-port! b)
              (k #f)))
        (let ((restored-b? (eq? (current-output-port) b)))
          (set-current-output-port! orig)
          (delete-file path)
          restored-b?)))

;; min/max: NaN is missing data, independent of argument order.
(test "max and min ignore NaN in every argument order"
      '(1.0 1.0 2.0 2.0 2.0 1.5 1.5 +inf.0 +inf.0 1.0 1.0 1.0 1.5 1.5 -inf.0 -inf.0)
      (list (max 1 +nan.0) (max +nan.0 1)
            (max 1 2 +nan.0) (max +nan.0 1 2) (max 1 +nan.0 2)
            (max 1.5 +nan.0) (max +nan.0 1.5)
            (max +inf.0 +nan.0) (max +nan.0 +inf.0)
            (min 1 +nan.0) (min +nan.0 1) (min 1 +nan.0 2)
            (min 1.5 +nan.0) (min +nan.0 1.5)
            (min -inf.0 +nan.0) (min +nan.0 -inf.0)))
(test "max and min of only NaN are NaN" '(#t #t #t)
      (list (nan? (max +nan.0)) (nan? (max +nan.0 +nan.0)) (nan? (min +nan.0 +nan.0))))
(test "max and min keep MIT signed-zero selection and exactness" '(#t #f #f #t 1/2 2.0 3 #t)
      (list (negative? (/ 1.0 (max 0.0 -0.0)))
            (negative? (/ 1.0 (max -0.0 0.0)))
            (negative? (/ 1.0 (min 0.0 -0.0)))
            (negative? (/ 1.0 (min -0.0 0.0)))
            (max 1/2 1/3)
            (max 1 2.0)
            (max 2 1 3)
            (guard (e (#t #t)) (max 'a 1) #f)))
(test "max over a long argument list" 99999
      (apply max (iota 100000)))

;; exact-integer-sqrt uses integer Newton iteration (bisection took over a
;; minute on (expt 10 20000)).
(test "exact-integer-sqrt small values"
      '((0 0) (1 0) (1 1) (1 2) (2 0) (3 6) (4 0) (4 1))
      (map (lambda (n) (call-with-values (lambda () (exact-integer-sqrt n)) list))
           '(0 1 2 3 4 15 16 17)))
(test "exact-integer-sqrt around 2^128"
      '((18446744073709551615 36893488147419103230)
        (18446744073709551616 0)
        (18446744073709551616 1)
        (99999999999999999999 199999999999999999998)
        (351364182882014 298878189871694))
      (map (lambda (n) (call-with-values (lambda () (exact-integer-sqrt n)) list))
           (list (- (expt 2 128) 1) (expt 2 128) (+ (expt 2 128) 1)
                 (- (expt 10 40) 1) 123456789012345678901234567890)))
(test "exact-integer-sqrt of huge values" '(#t #t #t)
      (list (call-with-values (lambda () (exact-integer-sqrt (expt 10 20000)))
              (lambda (s r) (and (= s (expt 10 10000)) (= r 0))))
            (call-with-values (lambda () (exact-integer-sqrt (- (expt 10 20000) 1)))
              (lambda (s r) (and (= s (- (expt 10 10000) 1))
                                 (= r (- (* 2 (expt 10 10000)) 2)))))
            (let ((k (+ (expt 3 30001) 7)))
              (call-with-values (lambda () (exact-integer-sqrt (- (* k k) 1)))
                (lambda (s r) (and (= s (- k 1)) (= r (* 2 (- k 1)))))))))
(test "exact-integer-sqrt rejects non-exact-nonnegative-integers" '(#t #t #t)
      (list (guard (e (#t #t)) (exact-integer-sqrt -1) #f)
            (guard (e (#t #t)) (exact-integer-sqrt 4.0) #f)
            (guard (e (#t #t)) (exact-integer-sqrt 1/2) #f)))
(test "integer-length uses a fast bit-length" '(0 1 8 9 0 8 9 66439 70000)
      (list (integer-length 0) (integer-length 1) (integer-length 255)
            (integer-length 256) (integer-length -1) (integer-length -256)
            (integer-length -257) (integer-length (expt 10 20000))
            (integer-length (- (expt 2 70000)))))

;; Before/after thunks run outside their own dynamic-wind frame, so escaping
;; from an after thunk does not unwind through (and re-run) that thunk.
(test "escaping from an after thunk runs it once" '(escaped 1)
      (let ((count 0))
        (let ((result
               (call-with-current-continuation
                 (lambda (outer)
                   (dynamic-wind
                     (lambda () #f)
                     (lambda () (outer 'first))
                     (lambda ()
                       (set! count (+ count 1))
                       (if (< count 5) (outer 'escaped))))))))
          (list result count))))
(test "re-entry rewinds before thunks once per entry" '(before body after before body after)
      (let ((log '()) (k #f) (n 0))
        (dynamic-wind
          (lambda () (set! log (cons 'before log)))
          (lambda ()
            (call-with-current-continuation (lambda (c) (set! k c)))
            (set! log (cons 'body log)))
          (lambda () (set! log (cons 'after log))))
        (set! n (+ n 1))
        (if (< n 2) (k #f))
        (reverse log)))

;; An empty parameterize still has a body: definitions stay local.
(parameterize () (define %stdlib-regression-parameterize-leak 1) 'ignored)
(test "parameterize () does not leak body definitions" 'undefined
      (guard (e (#t 'undefined)) %stdlib-regression-parameterize-leak))
(test "parameterize () body allows internal definitions" 20
      (parameterize () (define a 2) (* a 10)))

;;; ============================================================================
;;; Runtime regressions
;;; ============================================================================

(section "Runtime regressions")

;; #t and #f used to share atom ids with the symbols true and false.
(test "#t is not a symbol" #f (symbol? #t))
(test "'true is not #t" #f (eq? 'true #t))
(test "'false is not #f" #f (eqv? 'false #f))
(test "true, false and t are MIT-style variables" '(#t #f #t) (list true false t))
(test "case does not match 'true against #t" 'no (case 'true ((#t) 'hit) (else 'no)))
(test "memq #t skips the symbol true" #f (memq #t '(a true b)))
(test "equal? distinguishes (true) from (#t)" #f (equal? '(true) '(#t)))
(test "symbol->string rejects booleans" 'error (guard (e (#t 'error)) (symbol->string #f)))
(define-syntax %reg-true-pattern (syntax-rules () ((_ true) (list true #t))))
(test "a pattern variable named true leaves #t alone" '(5 #t) (%reg-true-pattern 5))
(test "string->symbol \"#t\" makes a symbol, not a boolean" '(#t #f)
      (let ((s (string->symbol "#t"))) (list (symbol? s) (eq? s #t))))

;; VM closures used to be pairs whose car was a code marker.
(define (%reg-closure x) (+ x 1))
(test "procedures are not pairs or lists" '(#f #f #t)
      (list (pair? %reg-closure) (list? %reg-closure) (procedure? %reg-closure)))
(test "pair? through apply on a procedure" #f (apply pair? (list %reg-closure)))
(test "car rejects a procedure" 'error (guard (e (#t 'error)) (car %reg-closure)))
(test "equal? on distinct closures of one lambda" #f
      (let ((mk (lambda (n) (lambda () n)))) (equal? (mk 1) (mk 1))))
(test "tree walk over a list holding a procedure terminates" 3
      (let count ((x (list 1 2 %reg-closure)))
        (cond ((pair? x) (+ (count (car x)) (count (cdr x))))
              ((null? x) 0)
              (else 1))))

;; Numbers
(test "modulo with an inexact integer operand" 4.0
      (let ((f (lambda (a b) (modulo a b)))) (f 1e10 7.0)))
(test "+ keeps negative zero" '(-0.0 -0.0) (list (+ -0.0) (+ -0.0 -0.0)))
(test "large inexact integers keep their decimal point" "9007199254740992.0"
      (number->string 9007199254740992.0))
(test "inexact integers round-trip as inexact" #f
      (exact? (string->number (number->string (exact->inexact 1234567890123457)))))
(test "two-argument log" '(3.0 2.0) (list (log 8 2) (log 100 10)))
(test "log of a bignum beyond the double range is finite" #t
      (let ((l (log (expt 10 400)))) (< 921 l 922)))
(test "expt of -1 to an odd bignum is exact -1" -1 (expt -1 (+ 1 (expt 10 30))))
(test "expt of 1 to a bignum stays exact" 1 (expt 1 (expt 10 30)))
(test "expt of an exact base to a huge exponent is an error" 'error
      (guard (e (#t 'error)) (expt 2 (expt 10 30))))
(test "expt of a huge exact base to a fractional power" 1e200 (expt (expt 10 400) 0.5))
(test "arithmetic-shift of zero by a huge count" 0 (arithmetic-shift 0 (expt 2 70)))

;; Strings and ports
(test "string input port follows mutation of its source" '(#\a #\x3BB #\a)
      (let* ((s (make-string 5 #\a)) (q (open-input-string s)))
        (let* ((a (read-char q))
               (b (begin (string-set! s 1 (integer->char #x3BB)) (read-char q)))
               (c (read-char q)))
          (list a b c))))
(test "reading a long string port is linear" 40000
      (let ((p (open-input-string (make-string 40000 #\a))))
        (let loop ((n 0)) (if (eof-object? (read-char p)) n (loop (+ n 1))))))
(test "displaying #\\null to a string port raises" 'error
      (guard (e (#t 'error)) (display (list #\a (integer->char 0)) (open-output-string))))
(test "textual file ports carry U+0000" '(#\a 0 #\b)
      (let ((path (temporary-file-path)))
        (call-with-output-file path
          (lambda (p) (write-char #\a p) (write-char (integer->char 0) p) (write-char #\b p)))
        (let ((r (call-with-input-file path
                   (lambda (p)
                     (let* ((a (read-char p)) (z (read-char p)) (b (read-char p)))
                       (list a (char->integer z) b))))))
          (delete-file path)
          r)))
(test "char-upper-case? uses the Uppercase property" '(#t #t)
      (list (char-upper-case? (integer->char #x24B6)) (char-upper-case? (integer->char #x2160))))
(test "char-lower-case? uses the Lowercase property" '(#t #t)
      (list (char-lower-case? (integer->char #xAA)) (char-lower-case? (integer->char #x2B0))))

;; Condition kinds and environments
(test "file-error? on a failed open" #t
      (guard (e ((file-error? e) #t) (#t #f)) (open-input-file "/nonexistent/vesper-regression")))
(test "read-error? on malformed input" #t
      (guard (e ((read-error? e) #t) (#t #f)) (read (open-input-string "(1 2"))))
(test "ordinary errors are neither file nor read errors" '(#f #f)
      (guard (e (#t (list (file-error? e) (read-error? e)))) (car 1)))
(test "environment binds an import under every name" '(1 3 9)
      (eval '(list (first '(1 2)) (car '(3)) (square 3))
            (environment '(rename (scheme base) (car first)) '(scheme base))))
(test "null-environment has derived syntax" 'a
      (eval '(case 1 ((1) 'a) (else 'b)) (null-environment 5)))
(test "null-environment has no procedures" 'unbound
      (guard (e (#t 'unbound)) (eval '(car '(1)) (null-environment 5))))
;; string-length/string-ref cache each buffer's character count (and an
;; all-ASCII flag) and the last non-ASCII index lookup. Mutation installs a
;; new buffer, so the cache must never show stale contents.
(test "string-ref and string-length follow mutation" '(4 #\a 4 #\x3BB #\a #\a error 4 #\b)
      (let ((s (make-string 4 #\a)))
        (let* ((a (string-length s)) (b (string-ref s 3)))
          (string-set! s 1 (integer->char #x3BB))
          (let* ((c (string-length s)) (d (string-ref s 1)) (e (string-ref s 2))
                 (f (string-ref s 3))
                 (g (guard (x (#t 'error)) (string-ref s 4))))
            (string-set! s 1 #\b)
            (list a b c d e f g (string-length s) (string-ref s 1))))))
(test "non-ASCII string-ref in any order" '(#\c #\x3BB #\x1F600 #\a #\b #\c 5)
      (let ((u (string #\x3BB #\a #\x1F600 #\b #\c)))
        (list (string-ref u 4) (string-ref u 0) (string-ref u 2) (string-ref u 1)
              (string-ref u 3) (string-ref u 4) (string-length u))))
(test "substring of a mutated string keeps its own contents" '("world" 5 #\w #\x3A9)
      (let* ((p (string-copy "hello world")) (v (substring p 6 11)))
        (string-set! p 6 (integer->char #x3A9))
        (list v (string-length v) (string-ref v 0) (string-ref p 6))))
(test "scanning a long string with string-ref is linear" 19400000
      (let* ((s (make-string 200000 #\a)) (n (string-length s)))
        (let loop ((i 0) (acc 0))
          (if (= i n) acc (loop (+ i 1) (+ acc (char->integer (string-ref s i))))))))
(test "scanning a long non-ASCII string with string-ref is linear" 95500000
      (let* ((s (make-string 100000 (integer->char #x3BB))) (n (string-length s)))
        (let loop ((i 0) (acc 0))
          (if (= i n) acc (loop (+ i 1) (+ acc (char->integer (string-ref s i))))))))
(test "with-output-to-string is a procedure taking a thunk" '(#t "hi1")
      (list (procedure? with-output-to-string)
            (with-output-to-string (lambda () (display "hi") (write 1)))))
(test "with-input-from-string calls a thunk and returns its value" '(42 (#\a #\b))
      (list (with-input-from-string "42 43" read)
            (with-input-from-string "ab" (lambda () (let ((x (read-char))) (list x (read-char)))))))
(test "with-output-to-string works through map" '("1" "2")
      (map with-output-to-string (list (lambda () (display 1)) (lambda () (display 2)))))
(test "complex numbers print like MIT Scheme" '("+i" "-i" "+2i" "1-i" "+1/2i" "3-2i" "1.5+1.0i")
      (map number->string
           (list (make-rectangular 0 1) (make-rectangular 0 -1) (sqrt -4)
                 (make-rectangular 1 -1) (make-rectangular 0 1/2)
                 (make-rectangular 3 -2) (make-rectangular 1.5 1.))))
(test "write of a pure imaginary matches number->string" "(+i -2i)"
      (call-with-output-string
        (lambda (p) (write (list (make-rectangular 0 1) (make-rectangular 0 -2)) p))))
(define (%reg-qq-tail g) `(,(g) 1))
(define (%reg-qq-tail-vec g) `#(0 ,(g)))
(define (%reg-qq-tail-apply) `(,(apply max '(1 2))))
(test "quasiquote in tail position builds its template" '((5 1) #(0 5) (2))
      (list (%reg-qq-tail (lambda () 5)) (%reg-qq-tail-vec (lambda () 5))
            (%reg-qq-tail-apply)))
(test "quasiquote transforms a vector in a dotted tail" '((a . #(5)) (a . #(1 5 5)))
      (let ((y 5)) (list `(a . #(,y)) `(a . #(1 ,@(list y y))))))

;; Interpreter
(define-syntax %reg-foo (syntax-rules () ((_) 'macro)))
(test "letrec body defines shadow a global macro" 'proc
  (letrec ((z 1)) (define (bar) (%reg-foo)) (define (%reg-foo) 'proc) (bar)))
(test "empty letrec body defines shadow a global macro" 'proc
  (letrec () (define (bar) (%reg-foo)) (define (%reg-foo) 'proc) (bar)))
(test "letrec re-entry restores earlier bindings" '(11 12 200 4)
  (let ((k #f) (n 0))
    (let ((r (letrec ((a 1) (b 2) (c (call/cc (lambda (c) (set! k c) 3))) (d 4))
               (set! a (+ a 10)) (set! b (+ b 10)) (list a b c d))))
      (set! n (+ n 1))
      (if (< n 3) (k (* n 100)) r))))
(test "letrec with four bindings keeps each in its slot" '(1 2 3 4)
  (letrec ((a 1) (b 2) (c 3) (d 4)) (list a b c d)))
(test "letrec body mixes defines and expressions" 6
  (letrec ((z 1)) (define a 5) (set! a (+ a 0)) (define b (+ a z)) b))
(test "shadowed keyword head is applied" 6 ((lambda (if) (if 1 2 3)) +))
(test "letrec-bound keyword head is applied" 42 (letrec ((or (lambda (a b) (* a b)))) (or 6 7)))

;; Reader and writer
(test "reader: labeled #t is a boolean" '(#t #t) (map boolean? (read (open-input-string "(#0=#t #0#)"))))
(test "reader: labeled #f is false" 'no (if (read (open-input-string "#0=#f")) 'yes 'no))
(test "reader: labeled booleans in source" '(#t #f) (list (boolean? '#0=#t) (if '#1=#f #t #f)))
(test "write: no labels for acyclic sharing" "((1 2) (1 2))" (let ((x (list 1 2))) (call-with-output-string (lambda (p) (write (list x x) p)))))
(test "display: no labels for acyclic sharing" "((1 2) (1 2))" (let ((x (list 1 2))) (call-with-output-string (lambda (p) (display (list x x) p)))))
(test "write: labels a cycle" "#0=(1 2 . #0#)" (let ((c (list 1 2))) (set-cdr! (cdr c) c) (call-with-output-string (lambda (p) (write c p)))))
(test "write: labels only the cycle" "((1 . #0=(2 3 . #0#)) (1 . #0#))" (let ((d (list 1 2 3))) (set-cdr! (cddr d) (cdr d)) (call-with-output-string (lambda (p) (write (list d d) p)))))
(test "display: symbols are not escaped" "Hello World" (call-with-output-string (lambda (p) (display (string->symbol "Hello World") p))))
(test "display: odd symbols in a list" "(ABC  1)" (call-with-output-string (lambda (p) (display (list (string->symbol "ABC") (string->symbol "") (string->symbol "1")) p))))
(test "write: symbols still escaped" "|Hello World|" (call-with-output-string (lambda (p) (write (string->symbol "Hello World") p))))
(test "format ~a: symbol unescaped" "Foo" (format #f "~a" (string->symbol "Foo")))
(test "reader: identifier ending in dot" 2 (length (read (open-input-string "(-a. b)"))))
(test "reader: -a. round-trips" #t (let ((s (string->symbol "-a."))) (eq? s (read (open-input-string (call-with-output-string (lambda (p) (write s p))))))))
(test "reader: +. is a symbol" #t (symbol? (read (open-input-string "+."))))
(test "reader: 5. is inexact 5" 5.0 (read (open-input-string "5.")))
(test "reader: ...x is one identifier" "...x" (symbol->string (read (open-input-string "...x"))))
(test "reader: 1/-2 is a symbol" #t (symbol? (read (open-input-string "1/-2"))))
(test "reader: 1/0 is an error" 'error (guard (e (#t 'error)) (read (open-input-string "1/0"))))
(test "string->number: 1i needs a sign" #f (string->number "1i"))
(test "string->number: 1/2i needs a sign" #f (string->number "1/2i"))
(test "string->number: +1i" (make-rectangular 0 1) (string->number "+1i"))
(test "reader: |\\x0;| reports its own error" "invalid string character scalar value" (guard (e ((error-object? e) (error-object-message e))) (read (open-input-string "|a\\x0;b|"))))
(test "reader: vertical line is a delimiter" '(a b c) (let ((p (open-input-string "a|b|c"))) (let* ((x (read p)) (y (read p)) (z (read p))) (list x y z))))
(test "string->number: radix 3" 3 (string->number "10" 3))
(test "string->number: radix 36" -1295 (string->number "-zz" 36))
(test "string->number: radix 7 rational" 5/7 (string->number "5/10" 7))
(test "string->number: prefix overrides radix" 16 (string->number "#x10" 3))
(test "string->number: bad digit for radix" #f (string->number "3" 3))
(test "string->number: radix 36 round trip" 123456789 (string->number (number->string 123456789 36) 36))
(test "string->number: #e polar is exact" #t (exact? (string->number "#e1e2@1")))
(test "string->number: #e polar, zero angle" 2 (string->number "#e2@0"))
(test "reader: many datum labels" #t (let* ((items (let loop ((i 0) (acc '())) (if (= i 3000) acc (loop (+ i 1) (cons (list i) acc))))) (data (append items items))) (equal? data (read (open-input-string (call-with-output-string (lambda (p) (write-shared data p))))))))
(test "reader: chained labels share" #t (let ((x (read (open-input-string "#0=#1=(#0# #1#)")))) (and (eq? (car x) x) (eq? (cadr x) x))))
(test "reader: large label number" #t (let ((x (read (open-input-string "(#300000=(a) #300000#)")))) (eq? (car x) (cadr x))))
(test "write: too-deep data raises" (quote raised) (guard (e (#t (quote raised))) (let loop ((i 0) (x (quote ()))) (if (< i 20000) (loop (+ i 1) (list x)) (begin (call-with-output-string (lambda (p) (write x p))) (quote returned))))))

;; Compiler and VM
;; B2: closures made by (define (g) ...) inside a letrec body see stack-local params
(define (reg-count-up n) (letrec ((start 0)) (define (step i) (if (< i n) (step (+ i 1)) i)) (step start)))
(test "letrec-body define closure sees parameter" 5 (reg-count-up 5))
(define reg-a 'global-a)
(define (reg-f4 reg-a) (letrec ((b 1)) (define (g) reg-a) (g)))
(test "letrec-body define closure does not see global" 44 (reg-f4 44))
;; B8: letrec / internal define shadowing a stack-local parameter
(define (reg-sh1 a) (letrec ((a 5)) a))
(test "letrec binding shadows parameter" 5 (reg-sh1 1))
(define (reg-sh2 a) (letrec ((b 2)) (define a 7) a))
(test "internal define in letrec body shadows parameter" 7 (reg-sh2 1))
(define (reg-sh3 a) (letrec ((a (lambda () 9))) (a)))
(test "letrec-bound procedure shadows parameter" 9 (reg-sh3 1))
(define (reg-sh4 a) (letrec ((a 1)) (set! a 2) a))
(test "set! of shadowing letrec binding" 2 (reg-sh4 0))
(define (reg-sh5 a) (letrec ((b 1)) (set! a 3)) a)
(test "set! of unshadowed parameter inside letrec" 3 (reg-sh5 0))
;; B3: macro-produced definitions stay local to the procedure
(define (reg-sum3 a) (define-values (x) (values 10)) (set! x (+ x a)) (if (> a 0) (+ (reg-sum3 (- a 1)) x) 0))
(test "define-values in recursive procedure is per-activation" 23 (reg-sum3 2))
(define-syntax reg-def2 (syntax-rules () ((_ n v) (define n v))))
(define (reg-h2 a) (reg-def2 reg-z 10) (+ a reg-z))
(test "macro-generated define works" 11 (reg-h2 1))
(test "macro-generated define does not leak" 'not-leaked (guard (e (#t 'not-leaked)) reg-z))
(define (reg-g5 a) (define-record-type reg-pt (reg-mk u) reg-pt? (u reg-pt-u)) (reg-pt-u (reg-mk a)))
(test "define-record-type inside procedure" 5 (reg-g5 5))
(test "define-record-type inside procedure does not leak" 'not-leaked (guard (e (#t 'not-leaked)) reg-mk))
;; B4: apply and call-with-values are proper tail calls
(define (reg-apply-loop n) (if (= n 0) 'done (apply reg-apply-loop (list (- n 1)))))
(test "apply in tail position does not grow the stack" 'done (reg-apply-loop 1100000))
(define (reg-cwv-loop n) (if (= n 0) 'done (call-with-values (lambda () (- n 1)) reg-cwv-loop)))
(test "call-with-values consumer is a tail call" 'done (reg-cwv-loop 600000))
(define (reg-values-loop n) (if (= n 0) 'done (call-with-values (lambda () (values (- n 1) 2)) (lambda (a b) (reg-values-loop a)))))
(test "call-with-values with two values in a loop" 'done (reg-values-loop 600000))
(test "call-with-values result in non-tail position" 3 (+ 1 (call-with-values (lambda () 2) (lambda (x) x))))
(define (reg-cwv-list a b) (list a (call-with-values (lambda () (values a b)) list) b))
(test "call-with-values keeps caller stack intact" '(1 (1 2) 2) (reg-cwv-list 1 2))
;; B5: first-class call-with-values
(test "call-with-values as a first-class value" '(1 2) (let ((cwv call-with-values)) (cwv (lambda () (values 1 2)) list)))
(test "map over call-with-values" '(3) (map call-with-values (list (lambda () (values 1 2))) (list +)))
(test "apply call-with-values" '(3 4) (apply call-with-values (list (lambda () (values 3 4)) list)))
;; B7: vector quasiquote with unquote-splicing
(test "vector quasiquote splicing (R7RS 4.2.8)" '#(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(test "vector quasiquote splicing empty" '#(start end) (let ((l '())) `#(start ,@l end)))
(test "nested vector quasiquote keeps inner splice" '(1 #(x 2 3) (quasiquote #((unquote-splicing (list 4))))) `(1 #(x ,@(list 2 3)) `#(,@(list 4))))
(define (forms->temp-file forms)
  (let ((path (temporary-file-path)))
    (call-with-output-file path
      (lambda (p) (for-each (lambda (f) (write f p) (newline p)) forms)))
    path))
(define load-escape-k #f)
(define load-after-escape #f)
(define load-escape-count 0)
(test "escaping from a loaded file runs the rest of the program once" '(escaped 1 #f)
      (let* ((path (forms->temp-file '((load-escape-k 'escaped) (set! load-after-escape #t))))
             (r (call/cc (lambda (k) (set! load-escape-k k) (load path) 'normal))))
        (set! load-escape-count (+ load-escape-count 1))
        (delete-file path)
        (list r load-escape-count load-after-escape)))
(test "guard catches an error raised by a loaded file" 'caught
      (let ((path (forms->temp-file '((car '())))))
        (let ((r (guard (e (#t 'caught)) (load path))))
          (delete-file path)
          r)))
(define load-reentry-k #f)
(define load-reentry-log '())
(test "continuation captured in a loaded file can be re-entered after load returns" '(3 2 1)
      (let ((path (forms->temp-file
                   '((set! load-reentry-log
                           (cons (call/cc (lambda (k) (set! load-reentry-k k) 1))
                                 load-reentry-log))))))
        (load path)
        (if (file-exists? path) (delete-file path))
        (if (< (length load-reentry-log) 3)
            (load-reentry-k (+ (length load-reentry-log) 1)))
        load-reentry-log))
(test "a macro defined in a loaded file is usable by later forms of that file" 2
      (let ((path (forms->temp-file
                   '((define-syntax load-twice (syntax-rules () ((_ e) (begin e e))))
                     (define load-macro-count 0)
                     (load-twice (set! load-macro-count (+ load-macro-count 1)))
                     load-macro-count))))
        (let ((r (load path)))
          (delete-file path)
          r)))
;; case-lambda in a restricted environment
(test "case-lambda in (environment '(scheme case-lambda))" 4 ((eval '(case-lambda ((x) x)) (environment '(scheme case-lambda))) 4))
;; eval requires its environment argument
(test "eval with one argument is an error" 'error (guard (e (#t 'error)) (eval '(+ 1 2))))

;;; ============================================================================
;;; Variable lookup caches
;;; ============================================================================
;;; The VM remembers where each variable reference last found its binding and
;;; reuses that until a binding for the name is added somewhere. Every case
;;; below runs a reference once to fill its cache, then changes what it
;;; should see.

(section "Variable lookup caches")

(define (ic-inc x) (+ x 1))
(test "assigning a primitive's name reaches cached call sites"
      '(2 0 plus 2)
      (let* ((saved +)
             (r1 (ic-inc 1)))
        (set! + -)
        (let ((r2 (ic-inc 1)))
          (set! + (lambda args 'plus))
          (let ((r3 (ic-inc 1)))
            (set! + saved)
            (list r1 r2 r3 (ic-inc 1))))))

(define ic-saved-car car)
(define (ic-first l) (car l))
(define ic-first-before (ic-first '(1 2)))
(define car cdr)
(define ic-first-redefined (ic-first '(1 2)))
(define car ic-saved-car)
(test "redefining a primitive at top level reaches cached call sites"
      '(1 (2) 1)
      (list ic-first-before ic-first-redefined (ic-first '(1 2))))

(define ic-x 'global)
(define (ic-define-after-use)
  (let ()
    (define (g) ic-x)
    (g)
    (define ic-x 'local)
    (g)))
(test "an internal define reaches a closure that already looked the name up"
      '(local local local)
      (list (ic-define-after-use) (ic-define-after-use) (ic-define-after-use)))

(define (ic-make flag)
  (let ()
    (if flag (define ic-x 'local))
    (lambda () ic-x)))
(test "activations that did and did not define the name"
      '(global local global local local global)
      (let* ((a (ic-make #t)) (b (ic-make #f)) (c (ic-make #t)))
        (list (b) (a) (b) (c) (a) (b))))
(test "activation defining the name after one that did not"
      '(local global local global)
      (let* ((d (ic-make #f)) (e (ic-make #t)))
        (list (e) (d) (e) (d))))

(define (ic-make-param flag n)
  (if flag (define ic-extra n))
  (lambda () n))
(test "a define in some activations moves the parameters of their frame"
      '(1 2 3 2 1)
      (let* ((m1 (ic-make-param #t 1)) (m2 (ic-make-param #f 2))
             (m3 (ic-make-param #t 3)))
        (list (m1) (m2) (m3) (m2) (m1))))

(define (ic-use-later) ic-defined-later)
(test "a global defined by eval after the reference first failed" 42
      (begin
        (guard (e (#t 'unbound)) (ic-use-later))
        (eval '(define ic-defined-later 42) (interaction-environment))
        (ic-use-later)))

(define (ic-adder n) (lambda (y) (+ y n)))
(define (ic-deep a) (lambda (b) (lambda (c) (lambda (d) (list a b c d (+ a b c d))))))
(test "closures over different activations of one frame"
      '((11 12 13 14) (1 2 3 4 10) (5 6 7 8 26))
      (list (map (lambda (f) (f 10)) (map ic-adder '(1 2 3 4)))
            ((((ic-deep 1) 2) 3) 4)
            ((((ic-deep 5) 6) 7) 8)))

(define ic-val 'global-val)
(test "a name both let-bound and global in one loop"
      '(global-val 4 global-val 2 global-val 0)
      (let loop ((i 0) (acc '()))
        (if (= i 6) acc
            (loop (+ i 1)
                  (cons (if (even? i) (let ((ic-val i)) ic-val) ic-val) acc)))))

(define (ic-letstar) (let* ((q 1) (+ (lambda (a b) (* a b 100))) (r (+ q 2))) r))
(test "let* rebinding + between uses of the global +" '(200 200 3)
      (list (ic-letstar) (ic-letstar) (+ 1 2)))

(define (ic-helper v) (list 'helper v))
(define-syntax ic-use-helper
  (syntax-rules () ((_ v) (ic-helper v))))
(define (ic-call-helper) (ic-use-helper 1))
(define (ic-shadow-helper) (let ((ic-helper (lambda (v) 'shadowed))) (ic-use-helper 2)))
(test "a macro's helper reference follows assignment and redefinition"
      '((helper 1) (new-helper 1) (new-helper 2) (redefined 1) (redefined 2))
      (let* ((h1 (ic-call-helper))
             (h2 (begin (set! ic-helper (lambda (v) (list 'new-helper v)))
                        (ic-call-helper)))
             (h3 (ic-shadow-helper)))
        (eval '(define ic-helper (lambda (v) (list 'redefined v)))
              (interaction-environment))
        (list h1 h2 h3 (ic-call-helper) (ic-shadow-helper))))

(define ic-cc-global 'before)
(define (ic-cc-run)
  (let ((k #f) (out '()))
    (let ((v (call-with-current-continuation (lambda (c) (set! k c) 0))))
      (set! out (cons (list v ic-cc-global) out))
      (if (= v 0)
          (begin (set! ic-cc-global 'after) (k 1))
          (reverse out)))))
(test "a reference re-entered through a continuation" '((0 before) (1 after))
      (ic-cc-run))

(define (ic-rest . args) (lambda () args))
(test "closures over rest-parameter frames" '((1 2) (3) (1 2))
      (let ((a (ic-rest 1 2)) (b (ic-rest 3)))
        (list (a) (b) (a))))
;;; ============================================================================
;;; car/cdr compositions, memq/memv/member, assq/assv/assoc, list-tail and
;;; list-ref as C primitives
;;; ============================================================================

(section "List primitives (cxr, member, assoc, list-tail, list-ref)")

(define (cxr-test-message thunk)
  (guard (e (#t (if (error-object? e) (error-object-message e) e)))
    (thunk)))

;; A full binary tree four levels deep whose leaves name their own path.
(define cxr-test-tree
  (let build ((depth 0) (path '()))
    (if (= depth 4)
        (reverse path)
        (cons (build (+ depth 1) (cons 'a path))
              (build (+ depth 1) (cons 'd path))))))
(test "two-level cxr"
      (list (car (car cxr-test-tree)) (car (cdr cxr-test-tree))
            (cdr (car cxr-test-tree)) (cdr (cdr cxr-test-tree)))
      (list (caar cxr-test-tree) (cadr cxr-test-tree)
            (cdar cxr-test-tree) (cddr cxr-test-tree)))
(test "three-level cxr"
      (list (car (car (car cxr-test-tree))) (car (car (cdr cxr-test-tree)))
            (car (cdr (car cxr-test-tree))) (car (cdr (cdr cxr-test-tree)))
            (cdr (car (car cxr-test-tree))) (cdr (car (cdr cxr-test-tree)))
            (cdr (cdr (car cxr-test-tree))) (cdr (cdr (cdr cxr-test-tree))))
      (list (caaar cxr-test-tree) (caadr cxr-test-tree)
            (cadar cxr-test-tree) (caddr cxr-test-tree)
            (cdaar cxr-test-tree) (cdadr cxr-test-tree)
            (cddar cxr-test-tree) (cdddr cxr-test-tree)))
(test "four-level cxr name their own path"
      '((a a a a) (d a a a) (a d a a) (d d a a) (a a d a) (d a d a)
        (a d d a) (d d d a) (a a a d) (d a a d) (a d a d) (d d a d)
        (a a d d) (d a d d) (a d d d) (d d d d))
      (map (lambda (f) (f cxr-test-tree))
           (list caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                 cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)))
(test "cxr through apply and map" '(2 (3) 3)
      (list (apply cadr '((1 2))) (car (map cddr '((1 2 3)))) (caddr '(1 2 3))))
(test "cxr errors name the procedure"
      '("cadr: not a pair" "cadr: not a pair" "caar: not a pair"
        "cdar: not a pair" "cddr: not a pair" "caddr: not a pair"
        "cdddr: not a pair" "cadddr: not a pair" "cddddr: not a pair"
        "cadr: not a pair")
      (map cxr-test-message
           (list (lambda () (cadr 5)) (lambda () (cadr '(1)))
                 (lambda () (caar '(1))) (lambda () (cdar '()))
                 (lambda () (cddr '(1 . 2))) (lambda () (caddr '(1 2)))
                 (lambda () (cdddr '(1 2))) (lambda () (cadddr '(1 2 3)))
                 (lambda () (cddddr '(1 2 3))) (lambda () (apply cadr '((1)))))))
(test "nested car/cdr report the step that failed" '("car: not a pair" "cdr: not a pair")
      (list (cxr-test-message (lambda () (car (cdr '(1)))))
            (cxr-test-message (lambda () (car (cdr 5))))))
(test "cxr arity is checked" '(error error)
      (list (guard (e (#t 'error)) (cadr))
            (guard (e (#t 'error)) (apply caddr '((1 2 3) (4))))))
(test "cxr in a standard-library environment" '(2 "cadr: not a pair" "car: not a pair" 3)
      (list (eval '(cadr '(1 2)) (environment '(scheme base)))
            (cxr-test-message
             (lambda () (eval '(cadr '(1)) (environment '(scheme base)))))
            (cxr-test-message
             (lambda () (eval '(car (cdr '(1))) (environment '(scheme base)))))
            (eval '(caddr '(1 2 3)) (environment '(scheme base) '(scheme cxr)))))

(test "memq/memv/member find the tail"
      '((c d) (1.5 2) ((1) 3) ("b") #f (#\b) (100000000000000000000 x))
      (list (memq 'c '(a b c d)) (memv 1.5 (list 1 1.5 2))
            (member (list 1) (list 2 (list 1) 3)) (member "b" (list "a" "b"))
            (memq 'z '(a b)) (memv #\b '(#\a #\b))
            (memv (expt 10 20) (list 1 (expt 10 20) 'x))))
(test "memq compares numbers and characters like eq?"
      '(#t #t #f)
      (list (pair? (memq 1000 (list 999 1000))) (pair? (memq #\x (list #\x)))
            (pair? (memq (list 1) (list (list 1))))))
(test "memv and member compare inexact numbers by value"
      '((2.5) (2.5) #f)
      (list (memv 2.5 (list 1 2.5)) (member 2.5 (list 1 2.5)) (memv 2 (list 2.0))))
(test "assq/assv/assoc find the entry"
      '((b 2) (2 . b) ("b" . 2) (#\y . 2) #f (1/2 . half) ((1 2) . list))
      (list (assq 'b '((a 1) (b 2))) (assv 2 '((1 . a) (2 . b)))
            (assoc "b" (list (cons "a" 1) (cons "b" 2)))
            (assv #\y '((#\x . 1) (#\y . 2))) (assq 'z '((a . 1)))
            (assv 1/2 (list (cons 1/3 'third) (cons 1/2 'half)))
            (assoc (list 1 2) (list (cons (list 1) 'one) (cons (list 1 2) 'list)))))
(test "member and assoc with a comparison procedure"
      '((2 3) (2 . b) #f (3) #f)
      (list (member 2.0 '(1 2 3) =) (assoc 2.0 '((1 . a) (2 . b)) =)
            (member 1 '() 5) (member 2 '(1 2 3) <) (assoc 1 '() 'not-a-procedure)))
(test "searches on the empty list" '(#f #f #f #f #f #f)
      (list (memq 'a '()) (memv 1 '()) (member 1 '())
            (assq 'a '()) (assv 1 '()) (assoc 1 '())))
(test "search errors"
      '("memq: improper list" "memv: improper list" "member: improper list"
        "assq: improper association list" "assv: improper association list"
        "assoc: improper association list" "assq: improper association list"
        "assoc: improper association list" "member: improper list"
        "member: expected procedure" "member: too many arguments"
        "assoc: improper association list")
      (map cxr-test-message
           (list (lambda () (memq 'x '(a . b))) (lambda () (memv 1 5))
                 (lambda () (member 1 '(2 . 3))) (lambda () (assq 1 5))
                 (lambda () (assv 1 '((2 . 3) . 4)))
                 (lambda () (assoc 1 '((2 . 3) . 4)))
                 (lambda () (assq 1 '((2 . 3) 5)))
                 (lambda () (assoc 1 '((2 . 3) 5) =))
                 (lambda () (member 1 '(2 . 3) =))
                 (lambda () (member 1 '(1) 5))
                 (lambda () (member 1 '(1) = =))
                 (lambda () (assoc 1 '(5))))))
(test "a match ahead of the bad tail is still found" '((1 . 2) (a . b))
      (list (memv 1 '(1 . 2)) (assq 'a '((a . b) . junk))))
(test "searches find a key in a circular list" '(b b (c . 3) c)
      (list (car (memq 'b (circular-list 'a 'b)))
            (car (member 'b (circular-list 'a 'b)))
            (assq 'c (circular-list '(a . 1) '(b . 2) '(c . 3)))
            (car (memv 'c (append '(x y) (circular-list 'a 'b 'c))))))
(test "searches reject a circular list without the key"
      '("memq: circular list" "memv: circular list" "member: circular list"
        "assq: circular list" "assv: circular list" "assoc: circular list"
        "memq: circular list")
      (map cxr-test-message
           (list (lambda () (memq 'z (circular-list 'a 'b)))
                 (lambda () (memv 9 (circular-list 1)))
                 (lambda () (member "z" (circular-list "a" "b" "c")))
                 (lambda () (assq 'z (circular-list '(a . 1))))
                 (lambda () (assv 9 (circular-list '(1 . 1) '(2 . 2))))
                 (lambda () (assoc "z" (circular-list (cons "a" 1))))
                 (lambda () (memq 'z (append (iota 1000) (circular-list 'a)))))))
(test "case still dispatches through memv" '(two other two)
      (map (lambda (x) (case x ((1) 'one) ((2 3.5) 'two) (else 'other)))
           (list 2 'q 3.5)))

(test "list-tail and list-ref" '((3 4) 3 () (1 2 . 3) 3 b c)
      (list (list-tail '(1 2 3 4) 2) (list-ref '(1 2 3 4) 2)
            (list-tail '(1 2) 2) (list-tail '(1 2 . 3) 0) (list-tail '(1 2 . 3) 2)
            (list-ref (circular-list 'a 'b 'c) 100)
            (list-ref (circular-list 'a 'b 'c) (+ (expt 10 30) 1))))
(test "list-tail and list-ref errors"
      '("list-tail: index out of bounds" "list-ref: index out of bounds"
        "list-ref: index out of bounds" "list-tail: expected nonnegative integer"
        "list-ref: expected nonnegative integer" "list-ref: expected nonnegative integer"
        "list-tail: index out of bounds" "list-ref: expected nonnegative integer")
      (map cxr-test-message
           (list (lambda () (list-tail '(1 2) 3)) (lambda () (list-ref '(1 2) 2))
                 (lambda () (list-ref '(1 2 . 3) 2)) (lambda () (list-tail '(1) -1))
                 (lambda () (list-ref '(1) 1.0)) (lambda () (list-ref '(1) 'x))
                 (lambda () (list-tail '(1 2) (expt 10 30)))
                 (lambda () (list-ref '(1) (- (expt 10 30)))))))
(test "list-set! still goes through list-tail" '(1 x 3)
      (let ((l (list 1 2 3))) (list-set! l 1 'x) l))

;; Redefining a primitive: code compiled before and after the definition, and
;; procedures passed around, all see the new binding; restoring the builtin
;; restores the fast path.
(define (cxr-redef-before x) (list (cadr x) (caddr x)))
(define (search-redef-before x l) (list (memq x l) (assq x l) (member x l)))
(define cxr-saved (list cadr caddr memq assq member))
(define (cadr x) 'user-cadr)
(define (caddr x) 'user-caddr)
(define (memq x l) 'user-memq)
(define (assq x l) 'user-assq)
(define (member x l . rest) 'user-member)
(define (cxr-redef-after x) (list (cadr x) (caddr x)))
(test "user redefinition of cxr and searches"
      '((user-cadr user-caddr) (user-cadr user-caddr)
        (user-memq user-assq user-member) (user-cadr))
      (list (cxr-redef-before '(1 2 3)) (cxr-redef-after '(1 2 3))
            (search-redef-before 'a '((a . 1))) (map cadr '((1 2)))))
(set! cadr (list-ref cxr-saved 0))
(set! caddr (list-ref cxr-saved 1))
(set! memq (list-ref cxr-saved 2))
(set! assq (list-ref cxr-saved 3))
(set! member (list-ref cxr-saved 4))
(test "restored primitives" '((2 3) (2 3) (#f (a . 1) #f))
      (list (cxr-redef-before '(1 2 3)) (cxr-redef-after '(1 2 3))
            (search-redef-before 'a '((a . 1)))))
(test "local bindings shadow the primitives" '(1 (1 2) mine)
      (let ((cadr car) (memq list) (assv (lambda (k l) 'mine)))
        (list (cadr '(1 2)) (memq 1 2) (assv 1 '((1 . 1))))))
;;; Let bindings in stack slots
;;; ============================================================================
;;; In a procedure whose parameters are stack locals, a let (the stdlib's
;;; let, let* and named let are inlined lambda applications) keeps bindings
;;; that no closure captures and nothing assigns in stack slots instead of an
;;; environment frame. Everything here must behave as with frames.

(section "Let bindings in stack slots")

(define (ls-closures n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (map (lambda (f) (f)) (reverse acc))
        (let ((j (* i 10)))
          (loop (+ i 1) (cons (lambda () j) acc))))))
(test "closures made in a loop keep their own let binding" '(0 10 20 30)
      (ls-closures 4))

(define (ls-counters n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        acc
        (let ((c i))
          (loop (+ i 1) (cons (lambda () (set! c (+ c 1)) c) acc))))))
(test "counters closed over let bindings are independent" '(5 2 1)
      (let ((cs (ls-counters 3)))
        ((car cs))
        ((car cs))
        (map (lambda (f) (f)) cs)))

(define (ls-setter n)
  (let ((x n) (y 0))
    (set! y (* x 2))
    (set! x (+ x y))
    (list x y)))
(test "set! on let bindings" '(15 10) (ls-setter 5))

;; ls-run-reentry calls thunk, then re-enters the continuation the code under
;; test saved twice more, collecting every value thunk's call returns. The
;; code under test saves it through a global, so none of its own closures
;; captures anything and its bindings are eligible for slots.
(define ls-saved-k #f)
(define (ls-save-k! k) (set! ls-saved-k k) 'k)
(define (ls-run-reentry thunk)
  (let ((n 0) (out '()))
    (set! ls-saved-k #f)
    (let ((v (thunk)))
      (set! out (cons v out))
      (set! n (+ n 1))
      (if (< n 3) (ls-saved-k n))
      (reverse out))))

(define (ls-reenter n)
  (let ((a (* n 2)) (b (+ n 1)))
    (let ((c (call/cc (lambda (k) (ls-save-k! k) 0))))
      (list a b c))))
(test "re-entering a let body restores its bindings"
      '((10 6 0) (10 6 1) (10 6 2))
      (ls-run-reentry (lambda () (ls-reenter 5))))

(define (ls-assigned-reentry)
  (let ((x 0))
    (call/cc ls-save-k!)
    (set! x (+ x 1))
    x))
(test "re-entry after set! of a let binding sees the assignment" '(1 2 3)
      (ls-run-reentry ls-assigned-reentry))

(define (ls-assigned-param x)
  (call/cc ls-save-k!)
  (set! x (+ x 1))
  x)
(test "re-entry after set! of a parameter sees the assignment" '(1 2 3)
      (ls-run-reentry (lambda () (ls-assigned-param 0))))

(define-syntax ls-inc!
  (syntax-rules () ((_ v) (set! v (+ v 1)))))
(define (ls-macro-assigned)
  (let ((x 0))
    (call/cc ls-save-k!)
    (ls-inc! x)
    x))
(test "re-entry after a macro's set! of a let binding" '(1 2 3)
      (ls-run-reentry ls-macro-assigned))

(define (ls-let-syntax-assigned x)
  (call/cc ls-save-k!)
  (let-syntax ((bump (syntax-rules () ((_ v) (set! v (+ v 1))))))
    (bump x))
  x)
(test "re-entry after a local macro's set! of a parameter" '(1 2 3)
      (ls-run-reentry (lambda () (ls-let-syntax-assigned 0))))

(define (ls-loop-reentry)
  (let loop ((i 0) (acc '()))
    (if (= i 3)
        (reverse acc)
        (let ((sq (* i i)) (tag (if (= i 1) (call/cc ls-save-k!) 'first)))
          (loop (+ i 1) (cons (list i sq tag) acc))))))
(test "re-entering one loop iteration restores its let bindings"
      '(((0 0 first) (1 1 k) (2 4 first))
        ((0 0 first) (1 1 1) (2 4 first))
        ((0 0 first) (1 1 2) (2 4 first)))
      (ls-run-reentry ls-loop-reentry))

(define (ls-shadow x)
  (let ((x (+ x 1)) (y x))
    (let ((x (* x 10)))
      (let* ((y (+ y 1)) (y (* y 2)))
        (list x y)))))
(test "nested lets shadowing a parameter and each other" '(20 4)
      (ls-shadow 1))
(define (ls-swap a b) (let ((a b) (b a)) (list a b)))
(test "let swapping parameters" '(2 1) (ls-swap 1 2))
(define (ls-lambda-shadow x)
  (let ((y (* x 2)))
    (map (lambda (y) (+ y 1)) (list y x))))
(test "lambda parameter shadowing a let binding" '(7 4) (ls-lambda-shadow 3))
(define (ls-keyword-names n)
  (let ((if list) (car cdr))
    (if n (car '(1 2)))))
(test "let binding keyword and primitive names" '(7 (2))
      (ls-keyword-names 7))
(define (ls-let* n) (let* ((x n) (x (+ x 1)) (x (* x x))) x))
(test "let* rebinding sequentially" 9 (ls-let* 2))

(define (ls-internal n)
  (let ((a n))
    (define b (* a 2))
    (define (c) (+ a b))
    (c)))
(test "internal defines in a let body" 9 (ls-internal 3))
(define (ls-internal-loop n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (let ((sq (* i i)))
          (define twice (* sq 2))
          (loop (+ i 1) (+ acc twice))))))
(test "internal define in a loop's let" 28 (ls-internal-loop 4))

(define-syntax ls-let1
  (syntax-rules () ((_ v e body ...) (let ((v e)) body ...))))
(define (ls-via-macro n) (ls-let1 t (* n n) (+ t 1)))
(test "macro expanding to let" 17 (ls-via-macro 4))

(define-syntax ls-define-in-body
  (syntax-rules () ((_ v e) (define v e))))
(define ls-y 'global)
(define (ls-macro-define n)
  (let ((a n))
    (ls-define-in-body ls-y (* a 3))
    (+ a ls-y)))
(test "macro expanding to define in a let body" 8 (ls-macro-define 2))
(define (ls-macro-define-empty)
  ((lambda ()
     (ls-define-in-body ls-y 5)
     (* ls-y 2))))
(test "macro expanding to define in an empty let" 10 (ls-macro-define-empty))
(test "those defines stayed local" 'global ls-y)
(define-syntax ls-def2
  (syntax-rules () ((_ a b v) (begin (define a v) (define b (+ a 1))))))
(define (ls-define-loop n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (let ((t (* i i)))
          (let ()
            (ls-def2 p q t)
            (loop (+ i 1) (cons (list p q) acc)))))))
(test "macro defines inside a loop's (let ())" '((0 1) (1 2) (4 5))
      (ls-define-loop 3))

(define-syntax ls-thunk-of (syntax-rules () ((_ e) (lambda () e))))
(define (ls-macro-closure n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (map (lambda (f) (f)) (reverse acc))
        (let ((v (* i 3)))
          (loop (+ i 1) (cons (ls-thunk-of v) acc))))))
(test "macro making a closure over a let binding" '(0 3 6)
      (ls-macro-closure 3))
(define (ls-local-macro-closure n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (map (lambda (f) (f)) (reverse acc))
        (let ((x (* i 2)))
          (let-syntax ((thunk (syntax-rules () ((_ v) (lambda () v)))))
            (loop (+ i 1) (cons (thunk x) acc)))))))
(test "local macro making a closure over a let binding" '(0 2 4)
      (ls-local-macro-closure 3))
(define (ls-promises n)
  (let loop ((i 0) (ps '()))
    (if (= i n)
        (map force (reverse ps))
        (let ((x (* i i)))
          (loop (+ i 1) (cons (delay x) ps))))))
(test "delay over a let binding in a loop" '(0 1 4 9) (ls-promises 4))
(define (ls-guarded n)
  (let ((x n))
    (guard (e (#t (list 'caught x e)))
      (raise (* x 2)))))
(test "guard over a let binding" '(caught 5 10) (ls-guarded 5))
(define ls-param (make-parameter 1))
(define (ls-parameterized n)
  (let ((x n))
    (parameterize ((ls-param (* x 10)))
      (+ x (ls-param)))))
(test "parameterize over a let binding" 22 (ls-parameterized 2))
(define (ls-values-loop n)
  (let loop ((i 0) (acc 0))
    (if (= i n)
        acc
        (let ((k (* i 2)))
          (let-values (((q r) (floor/ (+ k 7) 3)))
            (loop (+ i 1) (+ acc q r k)))))))
(test "let-values inside a loop's let" 41 (ls-values-loop 5))
(define (ls-case n)
  (let ((x (* n 2)))
    (case x ((2) 'two) ((4) 'four) (else x))))
(test "case on a let binding" '(two four 6) (map ls-case '(1 2 3)))

(define (ls-do-closures n)
  (do ((i 0 (+ i 1))
       (fs '() (cons (lambda () i) fs)))
      ((= i n) (map (lambda (f) (f)) fs))))
(test "do loop closures keep their own bindings" '(2 1 0) (ls-do-closures 3))
(define (ls-do-lets n)
  (do ((i 0 (+ i 1))
       (s 0 (let ((sq (* i i))) (+ s sq))))
      ((= i n) s)
    (let ((ignored (* i 2))) ignored)))
(test "do loop with lets in steps and body" 30 (ls-do-lets 5))
(define (ls-do-when n)
  (do ((i 0 (+ i 1))
       (s 0 (+ s (cond ((even? i) 1) ((= 0 (modulo i 3)) 2) (else 3)))))
      ((= i n) s)
    (when (< i 0) (display "neg"))))
(test "do loop with when" 18 (ls-do-when 10))

(define (ls-squares l)
  (if (null? l)
      '()
      (let ((x (car l)))
        (cons (* x x) (ls-squares (cdr l))))))
(test "recursion modulo cons through a let" '(1 4 9) (ls-squares '(1 2 3)))
(test "recursion modulo cons through a let, deep" 100000
      (length (ls-squares (iota 100000))))
(define (ls-count-down n)
  (let ((m (- n 1)))
    (if (= n 0) 'done (ls-count-down m))))
(test "tail call from a let body" 'done (ls-count-down 1000000))
(define (ls-even-odd n)
  (letrec ((ev? (lambda (k) (if (= k 0) #t (od? (- k 1)))))
           (od? (lambda (k) (if (= k 0) #f (ev? (- k 1))))))
    (let ((r (ev? n)))
      r)))
(test "letrec beside a let" '(#t #f) (list (ls-even-odd 10) (ls-even-odd 7)))
(define (ls-letrec* n)
  (letrec* ((a n) (b (* a 2)))
    (let ((c (+ a b)))
      c)))
(test "letrec* beside a let" 9 (ls-letrec* 3))
(define (ls-rest n)
  ((lambda args (let ((len (length args))) (list len args))) n (+ n 1)))
(test "inlined rest-parameter lambda" '(2 (1 2)) (ls-rest 1))
(define (ls-thunk)
  (let ((a 1))
    (let ((b (+ a 1)))
      (list a b (let ((c 3)) c)))))
(test "lets in a procedure without parameters" '(1 2 3) (ls-thunk))
(define (ls-args n)
  (+ (let ((a n)) (* a a)) (let ((b (+ n 1))) (* b b))))
(test "lets as operands" 13 (ls-args 2))
(define (ls-inits n)
  (let ((x (let ((t (* n 2))) (+ t 1)))
        (y (let ((t (* n 3))) (- t 1))))
    (list x y)))
(test "lets as initializers" '(5 5) (ls-inits 2))
(define-syntax ls-nest
  (syntax-rules ()
    ((_ () body) body)
    ((_ (v . vs) body) (let ((v 1)) (ls-nest vs body)))))
(define (ls-deep)
  (ls-nest (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
            a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34
            a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50
            a51 a52 a53 a54 a55 a56 a57 a58 a59 a60 a61 a62 a63 a64 a65 a66
            a67 a68 a69 a70)
           (+ a1 a35 a64 a65 a70)))
(test "more nested let bindings than slots" 5 (ls-deep))
(define (ls-gen-list n)
  (define return #f)
  (define resume #f)
  (define (gen)
    (let loop ((i 0))
      (when (< i n)
        (let ((v (* i i)))
          (call/cc (lambda (k) (set! resume k) (return v)))
          (loop (+ i 1)))))
    (return 'done))
  (let collect ((acc '()))
    (let ((x (call/cc (lambda (k) (set! return k)
                        (if resume (resume #f) (gen))))))
      (if (eq? x 'done) (reverse acc) (collect (cons x acc))))))
(test "generator re-entering a loop's let body" '(0 1 4 9 16)
      (ls-gen-list 5))
(define (ls-find-first pred l)
  (call/cc
   (lambda (return)
     (let loop ((l l))
       (if (null? l)
           #f
           (let ((x (car l)))
             (let ((y (* x x)))
               (if (pred y) (return x) (loop (cdr l))))))))))
(test "escaping from nested lets in a loop" 4
      (ls-find-first (lambda (y) (> y 10)) '(1 2 3 4 5)))
(define (ls-assign-loop-var n)
  (let loop ((i 0) (acc '()))
    (if (>= i n)
        (reverse acc)
        (begin
          (set! i (+ i 1))
          (loop (+ i 1) (cons i acc))))))
(test "assigning a named let variable" '(1 3 5) (ls-assign-loop-var 6))
(define ls-global 'global)
(define (ls-eval-let)
  (let ((ls-global 'local))
    (list ls-global (eval 'ls-global (interaction-environment)))))
(test "eval does not see a let binding" '(local global) (ls-eval-let))
(define (ls-letloop n)
  (let loop ((i 0) (acc 0))
    (if (= i n) acc
        (let ((sq (* i i)) (d (- i 1)))
          (let* ((a (+ sq d)) (b (quotient a 2)))
            (loop (+ i 1) (+ acc b)))))))
(test "loop over nested let and let*" 166665501 (ls-letloop 1000))
;;; MIT library procedures
;;; ============================================================================

(section "MIT library procedures")

(define (mitlib-error? thunk) (guard (e (#t #t)) (thunk) #f))
(define (mitlib-report thunk)
  (guard (e (#t (condition/report-string e))) (thunk) 'no-error))

;; fold-left, reduce-left, there-exists?, for-all?
(test "fold-left folds from the left, accumulator first"
      '((((() 1) 2) 3) 4) (fold-left list '() '(1 2 3 4)))
(test "fold-left of no elements is the initial value" 0 (fold-left + 0 '()))
(test "fold-left takes several lists"
      '(((() a . 1) b . 2) c . 3) (fold-left cons* '() '(a b c) '(1 2 3)))
(test "fold-left stops at the shortest list"
      '((() a 1 x) b 2 y) (fold-left list '() '(a b c) '(1 2) '(x y z)))
(test "fold-left subtracts left to right" -6 (fold-left - 0 '(1 2 3)))
(test "fold-left over a circular list stops at a finite one"
      '(((() 1 a) 2 b) 1 c)
      (fold-left list '() (circular-list 1 2) '(a b c)))
(test "fold-left on a long list" 4999950000 (fold-left + 0 (iota 100000)))
(test "fold-left does not call a non-procedure on no elements" 0
      (fold-left 3 0 '()))
(test "fold-left rejects improper and non-lists" '(#t #t #t #t)
      (list (mitlib-error? (lambda () (fold-left + 0 '(1 2 . 3))))
            (mitlib-error? (lambda () (fold-left + 0 'x)))
            (mitlib-error? (lambda () (fold-left list '() '(1 2) '(a b . c))))
            (mitlib-error? (lambda () (fold-left 3 0 '(1))))))
(test "reduce-left is left-associative" '((((1 2) 3) 4) 5)
      (reduce-left list '() '(1 2 3 4 5)))
(test "reduce-left edge cases" '(0 5 5 5)
      (list (reduce-left + 0 '()) (reduce-left + 0 '(5))
            (reduce-left list 'z '(5)) (reduce-left - 0 '(10 2 3))))
(test "reduce-left rejects improper lists" '(#t #t)
      (list (mitlib-error? (lambda () (reduce-left + 0 '(1 . 2))))
            (mitlib-error? (lambda () (reduce-left + 0 'x)))))
(test "there-exists? takes the list first and returns the predicate's value"
      '(#t 2 #f #f)
      (list (there-exists? '(1 2 3) even?)
            (there-exists? '(1 2 3) (lambda (x) (and (even? x) x)))
            (there-exists? '() even?)
            (there-exists? '(1 3) even?)))
(test "for-all? takes the list first and returns the predicate's value"
      '(#t 4 #t #f)
      (list (for-all? '(2 4) even?)
            (for-all? '(2 4) (lambda (x) (and (even? x) x)))
            (for-all? '() even?)
            (for-all? '(2 3) even?)))
(test "there-exists? and for-all? reject improper lists" '(#t #t #t #t)
      (list (mitlib-error? (lambda () (there-exists? '(1 2 . 4) even?)))
            (mitlib-error? (lambda () (for-all? '(2 4 . 5) even?)))
            (mitlib-error? (lambda () (there-exists? 'x even?)))
            (mitlib-error? (lambda () (for-all? '(1) 5)))))

;; list-head, sublist, except-last-pair(!)
(test "list-head" '((1 2) () (1 2 3 4) (1 2 3) ())
      (list (list-head '(1 2 3 4) 2) (list-head '(1 2 3 4) 0)
            (list-head '(1 2 3 4) 4) (list-head '(1 2 3 . 4) 3)
            (list-head 'x 0)))
(test "list-head returns a fresh list" #f
      (let ((l (list 1 2))) (eq? l (list-head l 2))))
(test "list-head errors" '(#t #t #t #t #t)
      (list (mitlib-error? (lambda () (list-head '(1 2 3 4) 5)))
            (mitlib-error? (lambda () (list-head '(1 2 3 . 4) 4)))
            (mitlib-error? (lambda () (list-head '(1 2) -1)))
            (mitlib-error? (lambda () (list-head '(1 2) 1.0)))
            (mitlib-error? (lambda () (list-head 'x 1)))))
(test "list-head on a long list" 99999 (length (list-head (iota 100000) 99999)))
(test "sublist" '((b c) (a b c d e) () (b c) ())
      (list (sublist '(a b c d e) 1 3) (sublist '(a b c d e) 0 5)
            (sublist '(a b c d e) 2 2) (sublist '(a b c . d) 1 3)
            (sublist 'x 0 0)))
(test "sublist errors" '(#t #t #t #t #t)
      (list (mitlib-error? (lambda () (sublist '(a b c d e) 3 2)))
            (mitlib-error? (lambda () (sublist '(a b c d e) 0 6)))
            (mitlib-error? (lambda () (sublist '(a b c d e) -1 2)))
            (mitlib-error? (lambda () (sublist '(a b c . d) 1 4)))
            (mitlib-error? (lambda () (sublist '(a b c) 1 'x)))))
(test "except-last-pair" '((1 2) () (1))
      (list (except-last-pair '(1 2 3)) (except-last-pair '(1))
            (except-last-pair '(1 2 . 3))))
(test "except-last-pair copies" '((1 2) (1 2 3))
      (let* ((l (list 1 2 3)) (r (except-last-pair l))) (list r l)))
(test "except-last-pair! cuts the list" '((1 2) (1 2) () (1) ())
      (let* ((l (list 1 2 3)) (r (except-last-pair! l)))
        (list r l (except-last-pair! (list 1))
              (except-last-pair! (cons 1 (cons 2 3)))
              (except-last-pair! (cons 1 2)))))
(test "except-last-pair errors" '(#t #t #t #t #t)
      (list (mitlib-error? (lambda () (except-last-pair '())))
            (mitlib-error? (lambda () (except-last-pair 'x)))
            (mitlib-error? (lambda () (except-last-pair! '())))
            (mitlib-error? (lambda () (except-last-pair! 'x)))
            (mitlib-error? (lambda () (except-last-pair (circular-list 1 2))))))
(test "except-last-pair on a long list" 99999
      (length (except-last-pair (iota 100000))))

;; make-initialized-list, list-of-type?, vector-of-type?, del-ass*
(test "make-initialized-list" '((0 1 4 9) ())
      (list (make-initialized-list 4 (lambda (i) (* i i)))
            (make-initialized-list 0 5)))
(test "make-initialized-list errors" '(#t #t)
      (list (mitlib-error? (lambda () (make-initialized-list -1 list)))
            (mitlib-error? (lambda () (make-initialized-list 3 5)))))
(test "list-of-type?" '(#t #f #f #t #f)
      (list (list-of-type? '(1 2 3) number?) (list-of-type? '(1 a 3) number?)
            (list-of-type? '(1 2 . 3) number?) (list-of-type? '() number?)
            (list-of-type? 5 number?)))
(test "vector-of-type?" '(#t #f #f #t)
      (list (vector-of-type? #(1 2) number?) (vector-of-type? #(1 a) number?)
            (vector-of-type? '(1) number?) (vector-of-type? #() number?)))
(test "del-assq, del-assv and del-assoc remove every match"
      '(((a . 1) (c . 3)) ((1 . a) (3 . d)) (("a" . 1)) ((b . 3)) ())
      (list (del-assq 'b '((a . 1) (b . 2) (c . 3) (b . 4)))
            (del-assv 2 '((1 . a) (2 . b) (2 . c) (3 . d)))
            (del-assoc "b" '(("a" . 1) ("b" . 2)))
            (del-assq 'a '((a . 1) (a 2) (b . 3)))
            (del-assq 'b '())))
(test "del-assq returns a fresh list" '(#f #t)
      (let* ((l (list (list 'a 1) (list 'b 2))) (r (del-assq 'c l)))
        (list (eq? r l) (eq? (car r) (car l)))))
(test "del-assq! and friends splice out matches"
      '((((b . 2)) ((a . 1) (b . 2))) () (("y" . 3)))
      (list (let* ((l (list (cons 'a 1) (cons 'b 2) (cons 'a 3)))
                   (r (del-assq! 'a l)))
              (list r l))
            (del-assv! 1 (list (cons 1 2)))
            (del-assoc! "x" (list (cons "x" 2) (cons "y" 3)))))
(test "del-ass procedures validate the whole alist" '(#t #t #t #t)
      (list (mitlib-error? (lambda () (del-assq 'b 'x)))
            (mitlib-error? (lambda () (del-assq 'a '((a . 1) x))))
            (mitlib-error? (lambda () (del-assq 'a '((a . 1) (b . 2) . x))))
            (mitlib-error? (lambda () (del-assq! 'a '(1))))))

;; Vectors
(test "make-initialized-vector" '(#(0 1 4 9) #())
      (list (make-initialized-vector 4 (lambda (i) (* i i)))
            (make-initialized-vector 0 3)))
(test "make-initialized-vector errors" '(#t #t #t)
      (list (mitlib-error? (lambda () (make-initialized-vector -1 list)))
            (mitlib-error? (lambda () (make-initialized-vector 1.0 list)))
            (mitlib-error? (lambda () (make-initialized-vector 2 3)))))
(test "vector-binary-search takes (vector key<? unwrap-key key)"
      '(5 #f 1 7 #f #f (3 . b) #f "banana")
      (let ((id (lambda (x) x)))
        (list (vector-binary-search #(1 3 5 7) < id 5)
              (vector-binary-search #(1 3 5 7) < id 4)
              (vector-binary-search #(1 3 5 7) < id 1)
              (vector-binary-search #(1 3 5 7) < id 7)
              (vector-binary-search #(1 3 5 7) < id 8)
              (vector-binary-search #() < id 7)
              (vector-binary-search #((1 . a) (3 . b) (5 . c)) < car 3)
              (vector-binary-search #((1 . a) (3 . b) (5 . c)) < car 4)
              (vector-binary-search (vector "apple" "banana" "cherry")
                                    string<? id "banana"))))
(test "vector-binary-search probes like MIT"
      '((9 7) (7 9) (9 11) (9 9) (9 9))
      (let ((log '()))
        (vector-binary-search #(1 3 5 7 9 11 13)
                              (lambda (a b) (set! log (cons (list a b) log)) (< a b))
                              (lambda (x) x) 9)
        (reverse log)))
(test "vector-binary-search errors" '(#t #t #t)
      (list (mitlib-error? (lambda () (vector-binary-search '(1 2) < car 5)))
            (mitlib-error? (lambda () (vector-binary-search #(1 3) 5 - (lambda (x) x))))
            (mitlib-error? (lambda () (vector-binary-search #(1 3) 3 -)))))
(test "subvector, vector-head and vector-tail"
      '(#(b c) #(a b c d e) #() #(a b) #() #(c d) #())
      (list (subvector #(a b c d e) 1 3) (subvector #(a b c d e) 0 5)
            (subvector #(a b c d e) 2 2) (vector-head #(a b c d) 2)
            (vector-head #(a b c d) 0) (vector-tail #(a b c d) 2)
            (vector-tail #(a b c d) 4)))
(test "subvector errors" '(#t #t #t #t #t #t #t)
      (list (mitlib-error? (lambda () (subvector #(a b c d e) 3 2)))
            (mitlib-error? (lambda () (subvector #(a b c d e) 0 6)))
            (mitlib-error? (lambda () (subvector #(a b c d e) -1 2)))
            (mitlib-error? (lambda () (subvector '(a) 0 0)))
            (mitlib-error? (lambda () (vector-head #(a b c d) 5)))
            (mitlib-error? (lambda () (vector-tail #(a b c d) 5)))
            (mitlib-error? (lambda () (vector-tail '(1) 0)))))
(test "vector-grow" '(5 #(a b c) #(a b c) #f)
      (let* ((v (vector 'a 'b 'c)) (g (vector-grow v 5)))
        (list (vector-length g) (subvector g 0 3) (vector-grow v 3)
              (eq? v (vector-grow v 3)))))
(test "vector-grow errors" '(#t #t #t)
      (list (mitlib-error? (lambda () (vector-grow #(a b c) 2)))
            (mitlib-error? (lambda () (vector-grow '(a) 2)))
            (mitlib-error? (lambda () (vector-grow #(1 2) 'x)))))
(test "vector-first and vector-last" '(1 3 #t #t)
      (list (vector-first #(1 2 3)) (vector-last #(1 2 3))
            (mitlib-error? (lambda () (vector-first #())))
            (mitlib-error? (lambda () (vector-last #())))))
(test "subvector->list and subvector-fill!" '((b c) #(1 x x 4 5) #t #t)
      (list (subvector->list #(a b c d) 1 3)
            (let ((v (vector 1 2 3 4 5))) (subvector-fill! v 1 3 'x) v)
            (mitlib-error? (lambda () (subvector->list #(a b c d) 1 5)))
            (mitlib-error? (lambda () (subvector-fill! (vector 1 2) 1 0 'x)))))
(test "subvector-move-left! and subvector-move-right!"
      '(#(a b 2 3 e) #(a b 2 3 e) #(2 3 4 4 5) #(1 1 2 3 5) #(1 1 1 1 5))
      (list (let ((w (vector 'a 'b 'c 'd 'e)))
              (subvector-move-left! (vector 1 2 3 4 5) 1 3 w 2) w)
            (let ((w (vector 'a 'b 'c 'd 'e)))
              (subvector-move-right! (vector 1 2 3 4 5) 1 3 w 2) w)
            (let ((v (vector 1 2 3 4 5))) (subvector-move-left! v 1 4 v 0) v)
            (let ((v (vector 1 2 3 4 5))) (subvector-move-right! v 0 3 v 1) v)
            (let ((v (vector 1 2 3 4 5))) (subvector-move-left! v 0 3 v 1) v)))
(test "subvector-move errors" '(#t #t #t #t)
      (list (mitlib-error? (lambda () (subvector-move-left! (vector 1 2) 0 3 (vector 1 2 3) 0)))
            (mitlib-error? (lambda () (subvector-move-left! (vector 1 2) 0 2 (vector 1 2 3) 2)))
            (mitlib-error? (lambda () (subvector-move-right! (vector 1 2) 0 2 '(1 2 3) 0)))
            (mitlib-error? (lambda () (subvector-move-right! (vector 1 2) 0 2 (vector 1 2 3) -1)))))

;; Streams
(define (mitlib-integers n) (cons-stream n (mitlib-integers (+ n 1))))
(test "cons-stream delays its second operand and forces it once" '(1 11)
      (let* ((n 0)
             (s (cons-stream (begin (set! n (+ n 1)) 'a)
                             (begin (set! n (+ n 10)) '())))
             (before n))
        (stream-cdr s)
        (stream-cdr s)
        (list before n)))
(test "stream-pair?" '(#t #f #t #t #f #f)
      (list (stream-pair? (mitlib-integers 0)) (stream-pair? (cons 1 2))
            (stream-pair? (cons 1 (delay 2))) (stream-pair? (cons 1 (make-promise 2)))
            (stream-pair? '()) (stream-pair? 5)))
(test "stream-car, stream-cdr, stream-first and stream-rest" '(0 1 0 1 2 ())
      (let ((s (mitlib-integers 0)))
        (list (stream-car s) (stream-car (stream-cdr s)) (stream-first s)
              (stream-first (stream-rest s))
              (stream-first (stream-rest (stream 1 2)))
              (stream-rest (stream 1)))))
(test "stream accessors reject non-streams" '(#t #t #t #t #t)
      (list (mitlib-error? (lambda () (stream-car '())))
            (mitlib-error? (lambda () (stream-cdr '())))
            (mitlib-error? (lambda () (stream-car (cons 1 2))))
            (mitlib-error? (lambda () (stream-first '())))
            (mitlib-error? (lambda () (stream-rest (cons 1 2))))))
(test "empty streams" '(#t #t #f #f () #t)
      (list (stream-null? '()) (empty-stream? the-empty-stream)
            (stream-null? (stream 1)) (empty-stream? 5) the-empty-stream
            (null? (stream))))
(test "stream-head and stream-tail" '((0 1 2 3 4) () () 10 (3))
      (let ((s (mitlib-integers 0)))
        (list (stream-head s 5) (stream-head s 0) (stream-tail (stream 1 2) 2)
              (stream-car (stream-tail s 10))
              (stream->list (stream-tail (stream 1 2 3) 2)))))
(test "stream-head and stream-tail errors" '(#t #t #t #t #t)
      (list (mitlib-error? (lambda () (stream-head (stream 1 2) 3)))
            (mitlib-error? (lambda () (stream-head (stream 1 2) -1)))
            (mitlib-error? (lambda () (stream-head (stream 1 2) 1.0)))
            (mitlib-error? (lambda () (stream-tail (stream 1 2) 3)))
            (mitlib-error? (lambda () (stream-tail '(1 2) 1)))))
(test "stream-ref" '(7 #t #t)
      (list (stream-ref (mitlib-integers 0) 7)
            (mitlib-error? (lambda () (stream-ref (stream 1 2) 2)))
            (mitlib-error? (lambda () (stream-ref 5 0)))))
(test "stream, list->stream and stream->list" '((1 2 3) () (a b c) #t ())
      (list (stream->list (stream 1 2 3)) (stream->list (stream))
            (stream->list (list->stream '(a b c)))
            (stream-pair? (list->stream '(1)))
            (list->stream '())))
(test "list->stream is lazy about a dotted tail" '(a #t)
      (let ((s (list->stream '(a b . c))))
        (list (stream-car s)
              (mitlib-error? (lambda () (stream->list s))))))
(test "stream->list takes an optional count" '((0 1 2 3) (1 2) ())
      (list (stream->list (mitlib-integers 0) 4) (stream->list (stream 1 2) 4)
            (stream->list (stream 1 2 3) 0)))
(test "stream->list does not force beyond its count" '(ok)
      (stream->list (cons-stream 'ok (error "forced")) 1))
(test "stream->list errors" '(#t #t #t #t)
      (list (mitlib-error? (lambda () (stream->list 5)))
            (mitlib-error? (lambda () (stream->list (cons-stream 1 2))))
            (mitlib-error? (lambda () (stream->list (stream 1) -1)))
            (mitlib-error? (lambda () (list->stream 'x)))))
(test "stream-length" '(3 0 #t #t)
      (list (stream-length (stream 1 2 3)) (stream-length (stream))
            (mitlib-error? (lambda () (stream-length (cons-stream 1 2))))
            (mitlib-error? (lambda () (stream-length '(1 2))))))
(test "stream-map, stream-filter and stream-for-each"
      '((0 2 4 6) (0 1 4 9) (11 22) (0 2 4 6) (2 4) (3 2 1) ((1 a) (2 b)))
      (let ((s (mitlib-integers 0)))
        (list (stream-head (stream-map + s s) 4)
              (stream-head (stream-map (lambda (x) (* x x)) s) 4)
              (stream->list (stream-map + (stream 1 2 3) (stream 10 20)))
              (stream-head (stream-filter even? s) 4)
              (stream->list (stream-filter even? (stream 1 2 3 4 5)))
              (let ((acc '()))
                (stream-for-each (lambda (x) (set! acc (cons x acc))) (stream 1 2 3))
                acc)
              (let ((acc '()))
                (stream-for-each (lambda (x y) (set! acc (cons (list x y) acc)))
                                 (stream 1 2 3) (stream 'a 'b))
                (reverse acc)))))
(test "stream-map and stream-filter on the empty stream" '(() ())
      (list (stream-map car '()) (stream-filter car '())))
(test "stream-map, stream-filter and stream-for-each reject non-streams" '(#t #t #t)
      (list (mitlib-error? (lambda () (stream-map + '(1 2))))
            (mitlib-error? (lambda () (stream-filter even? '(1 2))))
            (mitlib-error? (lambda () (stream-for-each display 5)))))
(test "stream-append" '((1 2 3) () (1 2 0 1) 5 1 #t)
      (list (stream->list (stream-append (stream 1 2) (stream 3) (stream)))
            (stream->list (stream-append))
            (stream-head (stream-append (stream 1 2) (mitlib-integers 0)) 4)
            (stream-append 5)
            (stream-car (stream-append (stream 1) 5))
            (mitlib-error? (lambda () (stream->list (stream-append (stream 1) 5))))))
(test "stream-append-map" '(1 1 2 2 3 3)
      (stream->list (stream-append-map (lambda (x) (stream x x)) (stream 1 2 3))))
(test "stream-accumulate is a right fold" '(6 (1 2 3) (1 (2 (3 z))) #t)
      (list (stream-accumulate + 0 (stream 1 2 3))
            (stream-accumulate cons '() (stream 1 2 3))
            (stream-accumulate list 'z (stream 1 2 3))
            (mitlib-error? (lambda () (stream-accumulate + 0 '(1))))))
(test "stream-last and stream-last-pair" '(3 (3) #t #t)
      (list (stream-last (stream 1 2 3))
            (stream->list (stream-last-pair (stream 1 2 3)))
            (mitlib-error? (lambda () (stream-last (stream))))
            (mitlib-error? (lambda () (stream-last-pair (cons-stream 1 2))))))
(test "stream-truncate" '((0 1 2 3 4) (1 2 3))
      (list (stream->list (stream-truncate (mitlib-integers 0) (lambda (x) (> x 4))))
            (stream->list (stream-truncate (stream 1 2 3) (lambda (x) #f)))))
(test "stream-write" '("{1 \"a\" 3}" "{}" "{a}" #t)
      (list (call-with-output-string (lambda (p) (stream-write (stream 1 "a" 3) p)))
            (call-with-output-string (lambda (p) (stream-write (stream) p)))
            (call-with-output-string
             (lambda (p) (parameterize ((current-output-port p)) (stream-write (stream 'a)))))
            (mitlib-error? (lambda () (stream-write (stream 1 2) 5)))))
(test "cons-stream*" '((1 2 3 4) (3 4) #t)
      (list (stream->list (cons-stream* 1 2 (stream 3 4)))
            (stream->list (cons-stream* (stream 3 4)))
            (stream-pair? (cons-stream* 1 2))))
(test "prime-numbers-stream" '(2 3 5 7 11 13 17 19 23 29)
      (stream-head prime-numbers-stream 10))
(test "long streams" '(10000 10000 10000 49995000 10000)
      (list (stream-ref (mitlib-integers 0) 10000)
            (length (stream-head (mitlib-integers 0) 10000))
            (stream-length (list->stream (iota 10000)))
            (stream-accumulate + 0 (list->stream (iota 10000)))
            (length (stream->list (stream-filter (lambda (x) #t)
                                                 (list->stream (iota 10000)))))))

;; Special forms
(test "named-lambda does not bind its name" '(8 #t)
      (list ((named-lambda (foo x) (* x 2)) 4)
            (mitlib-error? (lambda () ((named-lambda (mitlib-unbound-name x) (mitlib-unbound-name x)) 1)))))
(define mitlib-fluid 10)
(define (mitlib-get-fluid) mitlib-fluid)
(test "fluid-let rebinds for its dynamic extent" '((2 1) (20 10) 5)
      (list (let ((x 1)) (list (fluid-let ((x 2)) x) x))
            (list (fluid-let ((mitlib-fluid 20)) (mitlib-get-fluid)) (mitlib-get-fluid))
            (fluid-let () 5)))
(test "fluid-let swaps values on re-entry" '(30 10 31 10)
      (let ((k #f) (n 0) (log '()))
        (fluid-let ((mitlib-fluid 30))
          (call/cc (lambda (c) (set! k c)))
          (set! log (cons mitlib-fluid log))
          (set! mitlib-fluid (+ mitlib-fluid 1)))
        (set! log (cons mitlib-fluid log))
        (if (< n 1) (begin (set! n (+ n 1)) (k #f)))
        (reverse log)))
(test "fluid-let restores on an escape" 10
      (begin (guard (e (#t #f)) (fluid-let ((mitlib-fluid 7)) (error "escape")))
             mitlib-fluid))
(define-integrable (mitlib-square x) (* x x))
(test "define-integrable defines" 25 (mitlib-square 5))
(test "assert" '(ok "Assertion failed: (= 1 2)" "Assertion failed: #f \"ab\" 3")
      (list (begin (assert #t) (assert (> 5 3) "unused") 'ok)
            (mitlib-report (lambda () (assert (= 1 2))))
            (mitlib-report (lambda () (assert #f (string-append "a" "b") 3)))))

;; Miscellaneous
(test "write-line writes then ends the line" '("(1 \"a\" #\\b)\n" "\"x\"\ny\n")
      (list (call-with-output-string (lambda (p) (write-line '(1 "a" #\b) p)))
            (call-with-output-string
             (lambda (p) (parameterize ((current-output-port p)) (write-line "x") (write-line 'y))))))
(test "symbol-append" '(foobar || ab a1)
      (list (symbol-append 'foo 'bar) (symbol-append) (symbol-append 'a "b")
            (symbol-append 'a 1)))
(test "symbol<?" '(#t #f #f #t #t)
      (list (symbol<? 'a 'b) (symbol<? 'b 'a) (symbol<? 'a 'a)
            (mitlib-error? (lambda () (symbol<? 'a "b")))
            (mitlib-error? (lambda () (symbol<? 'a 'b 'c)))))
(test "char-ascii?" '(#t #t #f #f #t)
      (list (char-ascii? #\a) (char-ascii? #\x7f) (char-ascii? #\x80)
            (char-ascii? #\x100) (mitlib-error? (lambda () (char-ascii? 1)))))
(test "cube" '(27 8/27 3.375) (list (cube 3) (cube 2/3) (cube 1.5)))
(test "with-values" 3 (with-values (lambda () (values 1 2)) +))
(test "constant-procedure" '(5 5) (let ((k (constant-procedure 5))) (list (k) (k 1 2 3))))
(test "guarantee" '(5 "The object x, passed as an argument to foo, is not the correct type." "The object x is not the correct type.")
      (list (guarantee integer? 5 'foo)
            (mitlib-report (lambda () (guarantee integer? 'x 'foo)))
            (mitlib-report (lambda () (guarantee integer? 'x)))))
(test "MIT's hash-table/ names" '(1 none 1 (x) (1) 0 #t)
      (let ((h (make-equal-hash-table)))
        (hash-table/put! h 'a 1)
        (let* ((a (hash-table/get h 'a #f))
               (b (hash-table/get h 'b 'none))
               (n (hash-table/count h)))
          (hash-table/remove! h 'a)
          (hash-table/put! h 'x 1)
          (let ((keys (hash-table/key-list h)) (data (hash-table/datum-list h)))
            (hash-table/clear! h)
            (list a b n keys data (hash-table/count h)
                  (mitlib-error? (lambda () (hash-table/get h 'a))))))))

;; Timing
(test "runtime is CPU seconds as a flonum" '(#t #t)
      (let* ((a (runtime)) (b (runtime))) (list (inexact? a) (<= a b))))
(test "real-time is wall-clock milliseconds as a flonum" '(#t #t)
      (let ((t (real-time))) (list (inexact? t) (> t 1e12))))
(test "the tick clocks are exact" '(#t #t)
      (list (exact-nonnegative-integer? (real-time-clock))
            (exact-nonnegative-integer? (process-time-clock))))
(test "internal-time conversions" '(1.5 1500)
      (list (internal-time/ticks->seconds 1500) (internal-time/seconds->ticks 1.5)))
(test "with-timings returns the thunk's value after calling the receiver"
      '(done (#t #t #t))
      (let* ((got #f)
             (result (with-timings (lambda () 'done)
                                   (lambda (run gc real)
                                     (set! got (map exact-nonnegative-integer? (list run gc real)))
                                     'ignored))))
        (list result got)))
(test "with-timings passes multiple values through" '(1 2)
      (call-with-values (lambda () (with-timings (lambda () (values 1 2)) list)) list))

;; trace
(define (mitlib-traced-output thunk)
  (call-with-output-string
   (lambda (p) (parameterize ((trace-output-port p)) (thunk)))))
; The procedure numbers (MIT's hash numbers) depend on what was traced
; before; replace each with #.
(define (mitlib-unnumber s)
  (let loop ((i 0) (acc '()))
    (let ((j (string-search-forward "procedure " s i)))
      (if (not j)
          (apply string-append (reverse (cons (substring s i (string-length s)) acc)))
          (let skip ((k (+ j 10)))
            (if (and (< k (string-length s)) (char-numeric? (string-ref s k)))
                (skip (+ k 1))
                (loop k (cons "#" (cons (substring s i (+ j 10)) acc)))))))))
(define (mitlib-fact n) (if (= n 0) 1 (* n (mitlib-fact (- n 1)))))
(trace mitlib-fact)
(define mitlib-fact-trace (mitlib-traced-output (lambda () (mitlib-fact 2))))
(untrace mitlib-fact)
(test "trace reports entries and returns, recursion included"
      (string-append
       "[Entering #[compound-procedure # mitlib-fact]\n    Args: 2]\n"
       "[Entering #[compound-procedure # mitlib-fact]\n    Args: 1]\n"
       "[Entering #[compound-procedure # mitlib-fact]\n    Args: 0]\n"
       "[1\n      <== #[compound-procedure # mitlib-fact]\n    Args: 0]\n"
       "[1\n      <== #[compound-procedure # mitlib-fact]\n    Args: 1]\n"
       "[2\n      <== #[compound-procedure # mitlib-fact]\n    Args: 2]\n")
      (mitlib-unnumber mitlib-fact-trace))
(test "untrace restores the procedure" '("" 6)
      (list (mitlib-traced-output (lambda () (mitlib-fact 3))) (mitlib-fact 3)))
(define (mitlib-rest a . rest) (list a rest))
(trace-entry mitlib-rest)
(test "trace-entry prints a rest parameter as one argument" #t
      (let ((out (mitlib-traced-output (lambda () (mitlib-rest 1 2 3)))))
        (and (string-search-forward "    Args: 1\n          (2 3)]\n" out 0)
             (not (string-search-forward "<==" out 0))
             #t)))
(untrace-entry mitlib-rest)
(trace-exit mitlib-rest)
(test "trace-exit prints only the return" #t
      (let ((out (mitlib-traced-output (lambda () (mitlib-rest 1)))))
        (and (string-search-forward "[(1 ())\n      <== " out 0)
             (not (string-search-forward "Entering" out 0))
             #t)))
(trace-entry mitlib-rest)
(untrace-exit mitlib-rest)
(test "untrace-exit keeps entry tracing" #t
      (let ((out (mitlib-traced-output (lambda () (mitlib-rest 1)))))
        (and (string-search-forward "Entering" out 0)
             (not (string-search-forward "<==" out 0))
             #t)))
(untrace)
(test "untrace with no argument untraces everything" ""
      (mitlib-traced-output (lambda () (mitlib-rest 1))))
(define (mitlib-thunk) 'k)
(trace-both mitlib-thunk)
(test "a call without arguments has no Args line" #t
      (let ((out (mitlib-traced-output mitlib-thunk)))
        (and (string-search-forward "mitlib-thunk]]\n[k\n      <== " out 0) #t)))
(untrace mitlib-thunk)
(define (mitlib-long) (iota 30))
(trace mitlib-long)
(test "trace truncates long values like MIT" #t
      (let ((out (mitlib-traced-output mitlib-long)))
        (and (string-search-forward
              "[(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28...\n"
              out 0)
             #t)))
(untrace mitlib-long)
(define (mitlib-loop n) (if (= n 0) 'done (mitlib-loop (- n 1))))
(trace-entry mitlib-loop)
(test "trace-entry of a loop reports each call" '(done 4)
      (let* ((r #f)
             (out (mitlib-traced-output (lambda () (set! r (mitlib-loop 3))))))
        (list r (let count ((i 0) (n 0))
                  (let ((j (string-search-forward "[Entering" out i)))
                    (if j (count (+ j 1) (+ n 1)) n))))))
(untrace mitlib-loop)
(test "tracing a builtin does not trace the tracer"
      '(1 "[Entering #[compiled-procedure # car]\n    Args: (1 2)]\n[1\n      <== #[compiled-procedure # car]\n    Args: (1 2)]\n")
      (let ((out (open-output-string)) (r #f))
        (parameterize ((trace-output-port out))
          (trace car)
          (set! r (car '(1 2)))
          (untrace car))
        (list r (mitlib-unnumber (get-output-string out)))))
(test "trace and untrace errors" '(#t #t #t)
      (list (mitlib-error? (lambda () (untrace mitlib-fact)))
            (let ((five 5)) (mitlib-error? (lambda () (trace five))))
            (let ((five 5)) (mitlib-error? (lambda () (untrace-entry five))))))
(test "trace of an escaping procedure prints only the entry" #t
      (let ((boom (lambda () (car 5))))
        (trace boom)
        (let ((out (mitlib-traced-output (lambda () (guard (e (#t #f)) (boom))))))
          (untrace boom)
          (and (string-search-forward "Entering" out 0)
               (not (string-search-forward "<==" out 0))
               #t))))
(test "trace of a local binding" '(4 #t)
      (let ((double (lambda (x) (* 2 x))))
        (trace double)
        (let* ((r #f) (out (mitlib-traced-output (lambda () (set! r (double 2))))))
          (untrace double)
          (list r (and (string-search-forward "compound-procedure" out 0) #t)))))

;; pp
(define (mitlib-pp object . options)
  (call-with-output-string (lambda (p) (apply pp object p options))))
(test "pp prints what fits flat" '("(1 2 3)\n" "\"string\"\n" "sym\n" "#(1 2 3)\n" "()\n" "'x\n" "`(a ,b ,@c)\n")
      (map mitlib-pp (list '(1 2 3) "string" 'sym '#(1 2 3) '() ''x '`(a ,b ,@c))))
(test "pp breaks code like MIT"
      (string-append
       "(define (f x)\n"
       "  (let ((y (* x x)) (z (+ x x)))\n"
       "    (if (> y z)\n"
       "        (list y z)\n"
       "        (begin (display \"z is larger than y in this case\")\n"
       "               (newline)\n"
       "               (list z y)))))\n")
      (mitlib-pp '(define (f x) (let ((y (* x x)) (z (+ x x))) (if (> y z) (list y z) (begin (display "z is larger than y in this case") (newline) (list z y)))))))
(test "pp forces if, cond, and, or and procedure bodies onto lines"
      '("(if a\n    b\n    c)\n" "(lambda (x)\n  x)\n" "(define x\n  1)\n"
        "(let loop ((i 0))\n  (loop i))\n" "(case x\n  (else 1))\n" "(lambda (x))\n"
        "(begin a b)\n" "(1 (if a b c))\n")
      (map mitlib-pp '((if a b c) (lambda (x) x) (define x 1) (let loop ((i 0)) (loop i))
                       (case x (else 1)) (lambda (x)) (begin a b) (1 (if a b c)))))
(test "pp aligns a long combination under its first argument"
      (string-append "(aaaaaaaaaa bbbbbbbbbbb\n            ccccccccccc\n            ddddddddddd\n"
                     "            eeeeeeeeeee\n            fffffffffff\n            ggggggggggg)\n")
      (mitlib-pp '(aaaaaaaaaa bbbbbbbbbbb ccccccccccc ddddddddddd eeeeeeeeeee fffffffffff ggggggggggg)))
(test "pp lays out data as a table"
      '("#(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14\n  15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)\n"
        "(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14\n 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)\n")
      (list (mitlib-pp (list->vector (iota 30)))
            (mitlib-pp (iota 30) #f)))
(test "pp takes as-code? and indentation" '("(1 2 (quote a))\n" "    (if a\n        b\n        c)\n")
      (list (mitlib-pp '(1 2 'a) #f) (mitlib-pp '(if a b c) #t 4)))
(test "pp prints a dotted tail" "(f \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"\n   .\n   \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\")\n"
      (mitlib-pp (cons 'f (cons (make-string 40 #\a) (make-string 40 #\b)))))
(test "pp writes circular structure instead of looping" #t
      (let ((c (list 1 2 3)))
        (set-cdr! (cddr c) c)
        (string? (mitlib-pp c))))
(test "pp follows a record with its fields" "#[mitlib-point 1]\n(x 1)\n(y (a b))\n"
      (let ()
        (define-record-type mitlib-point (make-mitlib-point x y) mitlib-point?
          (x mitlib-point-x) (y mitlib-point-y))
        (let ((out (mitlib-pp (make-mitlib-point 1 '(a b)))))
          ;; The record's hash number varies; normalize it.
          (let ((i (string-search-forward " " out 0))
                (j (string-search-forward "]" out 0)))
            (string-append (substring out 0 (+ i 1)) "1"
                           (substring out j (string-length out)))))))
(test "pp to the current output port" "(a b)\n"
      (call-with-output-string
       (lambda (p) (parameterize ((current-output-port p)) (pp '(a b))))))
(test "pp errors" '(#t #t)
      (list (mitlib-error? (lambda () (pp '(a) 5)))
            (mitlib-error? (lambda () (pp)))))

;;; ============================================================================
;;; Primitive arity (checked from primitive_table.c) and procedure-arity
;;; ============================================================================

(define (arity-test-message thunk)
  (guard (e (#t (if (error-object? e) (error-object-message e) e)))
    (thunk)))
(test "arity errors from the primitive table, across modules"
      '("car: too few arguments (expected 1, got 0)"
        "cons: too many arguments (expected at most 2)"
        "vector-set!: too few arguments (expected 3, got 2)"
        "string-ref: too many arguments (expected at most 2)"
        "char-upcase: too few arguments (expected 1, got 0)"
        "sqrt: too many arguments (expected at most 1)"
        "atan: too many arguments (expected at most 2)"
        "make-rectangular: too few arguments (expected 2, got 1)"
        "quotient: too few arguments (expected 2, got 1)"
        "exact->inexact: too many arguments (expected at most 1)"
        "open-input-string: too many arguments (expected at most 3)"
        "hash-table-set!: too few arguments (expected 3, got 2)"
        "bytevector-copy!: too many arguments (expected at most 5)"
        "pair?: too many arguments (expected at most 1)"
        "list-tail: too few arguments (expected 2, got 1)"
        "cadr: too many arguments (expected at most 1)"
        "string-upcase: too few arguments (expected 1, got 0)"
        "char-ready?: too many arguments (expected at most 1)"
        "eof-object: too many arguments (expected at most 0)")
      (map arity-test-message
           (list (lambda () (car))
                 (lambda () (cons 1 2 3))
                 (lambda () (vector-set! (vector 1) 0))
                 (lambda () (string-ref "abc" 0 1))
                 (lambda () (char-upcase))
                 (lambda () (sqrt 4 4))
                 (lambda () (atan 1 1 1))
                 (lambda () (make-rectangular 1))
                 (lambda () (quotient 7))
                 (lambda () (exact->inexact 1 2))
                 (lambda () (open-input-string "a" 0 1 2))
                 (lambda () (hash-table-set! (make-equal-hash-table) 1))
                 (lambda () (bytevector-copy! (bytevector 1) 0 (bytevector 2)
                                              0 1 9))
                 (lambda () (pair? 1 2))
                 (lambda () (list-tail '(1)))
                 (lambda () (cadr '(1 2) 3))
                 (lambda () (string-upcase))
                 (lambda () (char-ready? (current-input-port) 1))
                 (lambda () (eof-object 1)))))
(test "arity errors when the primitive is applied first-class"
      '("car: too many arguments (expected at most 1)"
        "vector-ref: too few arguments (expected 2, got 0)"
        "member: too few arguments (expected 2, got 1)"
        "string-length: too many arguments (expected at most 1)")
      (map arity-test-message
           (list (lambda () (apply car '((1) (2))))
                 (lambda () (apply vector-ref '()))
                 (lambda () (%member 1))
                 (lambda () (let ((f string-length)) (f "a" "b"))))))
(test "primitives that report their own count keep their messages"
      '("-: requires at least one argument" "/: requires at least one argument"
        "char comparison: expected at least 2 arguments, got 1"
        "string comparison: expected at least 2 arguments, got 0")
      (map arity-test-message
           (list (lambda () (-)) (lambda () (/))
                 (lambda () (char<? #\a)) (lambda () (string=?)))))
(test "variadic primitives take any number of arguments" '(0 10 () (1 2 3 4))
      (list (+) (apply + '(1 1 1 1 1 1 1 1 1 1)) (append) (list 1 2 3 4)))
(test "procedure-arity of primitives"
      '((1 . 1) (2 . 2) (0 . #f) (1 . #f) (1 . 2) (2 . 2) (1 . 3) (3 . 5)
        (1 . 1) (2 . 2))
      (map procedure-arity
           (list car cons + - number->string vector-ref string-copy
                 bytevector-copy! call/cc call-with-values)))
(test "procedure-arity of closures"
      '((0 . 0) (2 . 2) (1 . #f) (0 . #f) (3 . 3))
      (list (procedure-arity (lambda () 1))
            (procedure-arity (lambda (a b) a))
            (procedure-arity (lambda (a . rest) a))
            (procedure-arity (lambda args args))
            (let ()
              (define (three a b c) (list a b c))
              (procedure-arity three))))
(test "procedure-arity of stdlib procedures and continuations"
      '((2 . #f) (0 . #f) #t)
      (list (procedure-arity map)
            (call/cc procedure-arity)
            (pair? (procedure-arity procedure-arity))))
(test "procedure-arity rejects a non-procedure"
      '("procedure-arity: not a procedure"
        "procedure-arity: too few arguments (expected 1, got 0)")
      (map arity-test-message
           (list (lambda () (procedure-arity 5))
                 (lambda () (procedure-arity)))))

;;; ============================================================================
;;; Unassigned variables
;;; ============================================================================

;; A letrec/letrec*/named-let variable or internal definition is bound, but
;; unassigned, until its initializer has run. Reading it then is the error
;; "Unassigned variable: NAME" (MIT Scheme's), not a placeholder value.

(section "Unassigned variables")

(define (ua-msg thunk)
  (guard (e ((error-object? e) (error-object-message e))
            (#t (list 'non-error e)))
    (thunk)))

(test "letrec: forward reference" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x y) (y 1)) x))))
(test "letrec: self reference" "Unassigned variable: x"
      (ua-msg (lambda () (letrec ((x (+ x 1))) x))))
(test "letrec: reference inside a constructor" "Unassigned variable: x"
      (ua-msg (lambda () (letrec ((v (vector 1 x)) (x 2)) v))))
(test "letrec: through a procedure called early" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((f (lambda () y)) (z (f)) (y 1)) z))))
(test "letrec: unassigned in operator position" "Unassigned variable: g"
      (ua-msg (lambda () (letrec ((x (g)) (g (lambda () 1))) x))))
(test "letrec: in a test position" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x (if y 1 2)) (y #t)) x))))
(test "letrec: read by map's procedure" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x (map (lambda (i) y) '(1 2))) (y 1)) x))))
(test "letrec: read by apply's procedure" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x (apply (lambda () y) '())) (y 1)) x))))
(test "letrec: mutual recursion still works" #t
      (letrec ((ev? (lambda (n) (if (= n 0) #t (od? (- n 1)))))
               (od? (lambda (n) (if (= n 0) #f (ev? (- n 1))))))
        (ev? 100)))
(test "letrec: closure over a later binding" 1
      (letrec ((x (lambda () y)) (y 1)) (x)))
(test "letrec: earlier binding visible to a later init" 2
      (letrec ((a 1) (b (+ a 1))) b))
(test "letrec: nested, inner forward reference" "Unassigned variable: z"
      (ua-msg (lambda () (letrec ((x (letrec ((y z) (z 1)) y))) x))))
(test "letrec: inner binding shadows an assigned outer one"
      "Unassigned variable: x"
      (ua-msg (lambda () (letrec ((x 1)) (letrec ((y x) (x 2)) y)))))
(test "letrec: the variable escapes once assigned" 5
      (let ((p (letrec ((x (lambda () y)) (y 5)) x))) (p)))
(test "letrec*: forward reference through a procedure"
      "Unassigned variable: b"
      (ua-msg (lambda () (letrec* ((a (lambda () b)) (c (a)) (b 1)) c))))
(test "letrec*: sequential inits" '(1 2 3)
      (letrec* ((a 1) (b (+ a 1)) (c (+ b 1))) (list a b c)))
(test "letrec*: forward reference" "Unassigned variable: y"
      (ua-msg (lambda () (letrec* ((x y) (y 1)) x))))
(test "a macro's renamed reference reports the variable's own name"
      "Unassigned variable: w"
      (ua-msg (lambda ()
                (letrec ((z (let-syntax ((m (syntax-rules () ((_) w))))
                              (m)))
                         (w 2))
                  z))))

(test "set! of an unassigned variable" 5
      (letrec ((x (begin (set! x 5) x))) x))
(test "set! of an unassigned variable, then the init assigns" 7
      (letrec ((x (begin (set! x 5) 7))) x))
(test "set! through a procedure before the init" 5
      (letrec ((f (lambda () (set! y 5) y)) (z (f)) (y z)) y))

(define (ua-define-self) (define a (+ a 1)) a)
(test "internal define: self reference" "Unassigned variable: a"
      (ua-msg ua-define-self))
(define (ua-define-later) (define a 1) (define b (+ b a)) b)
(test "internal define: later self reference" "Unassigned variable: b"
      (ua-msg ua-define-later))
(define (ua-define-forward) (define (a) c) (define b (a)) (define c 2) b)
(test "internal define: forward reference through a procedure"
      "Unassigned variable: c" (ua-msg ua-define-forward))
(define (ua-define-direct) (define a b) (define b 2) a)
(test "internal define: direct forward reference" "Unassigned variable: b"
      (ua-msg ua-define-direct))
(define (ua-define-param x) (define x (+ x 1)) x)
(test "internal define shadows a parameter" "Unassigned variable: x"
      (ua-msg (lambda () (ua-define-param 1))))
(define (ua-define-param2 x) (define (g) x) (define y (g)) (define x 10) y)
(test "internal define shadows a parameter, read through a procedure"
      "Unassigned variable: x" (ua-msg (lambda () (ua-define-param2 1))))
(define (ua-define-param3 x) (define (g) x) (define x 10) (g))
(test "internal define of a parameter, read after" 10 (ua-define-param3 1))
(test "internal define in let ()" "Unassigned variable: c"
      (ua-msg (lambda ()
                (let () (define a 1) (define b (lambda () c)) (define c (b))
                  c))))
(test "internal define in a let body shadowing its variable"
      "Unassigned variable: x"
      (ua-msg (lambda () (let ((x 1)) (define x (+ x 1)) x))))
(test "internal define in a letrec body" "Unassigned variable: q"
      (ua-msg (lambda () (letrec ((p 1)) (define r q) (define q p) r))))
(define (ua-define-mutual n)
  (define (ev? n) (if (= n 0) #t (od? (- n 1))))
  (define (od? n) (if (= n 0) #f (ev? (- n 1))))
  (ev? n))
(test "internal defines: mutual recursion" #t (ua-define-mutual 10))
(define (ua-define-loop n)
  (define (loop i acc) (if (= i n) acc (loop (+ i 1) (+ acc i))))
  (define total (loop 0 0))
  total)
(test "internal defines: recursive helper then a use" 45 (ua-define-loop 10))
(define (ua-define-each-call flag)
  (define (get) v)
  (define w (if flag (get) 'skipped))
  (define v 'set)
  (list w (get)))
(test "internal defines are unassigned again on each call"
      (list '(skipped set) "Unassigned variable: v" '(skipped set))
      (list (ua-define-each-call #f)
            (ua-msg (lambda () (ua-define-each-call #t)))
            (ua-define-each-call #f)))

(test "named let" 3 (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i)))
(test "named let: name not visible to the inits" "symbol"
      (let ((loop 'symbol))
        (let loop ((x (symbol->string loop))) x)))
(test "do loop" '(3 (2 1 0))
      (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 3) (list i acc))))

(test "error object: message and irritants"
      '(#t "Unassigned variable: y" ())
      (guard (e (#t (list (error-object? e) (error-object-message e)
                          (error-object-irritants e))))
        (letrec ((x y) (y 1)) x)))
(test "condition/report-string" "Unassigned variable: y"
      (guard (e (#t (condition/report-string e)))
        (letrec ((x y) (y 1)) x)))
(test "with-exception-handler sees the error" "Unassigned variable: y"
      (call-with-current-continuation
       (lambda (k)
         (with-exception-handler
          (lambda (e) (k (error-object-message e)))
          (lambda () (letrec ((x y) (y 1)) x))))))
(test "unbound is still undefined variable" "undefined variable: ua-nowhere"
      (ua-msg (lambda () ua-nowhere)))

;; Re-entering a letrec init with a continuation: the bindings assigned when
;; it was captured stay assigned.
(define ua-k #f)
(define ua-n 0)
(test "call/cc re-entry into a letrec init" '(3 12 12)
      (letrec ((a (call-with-current-continuation
                   (lambda (k) (set! ua-k k) 1)))
               (b (lambda () a)))
        (set! ua-n (+ ua-n 1))
        (if (< ua-n 3) (ua-k (+ ua-n 10)) (list ua-n a (b)))))
(define ua-r '())
(define ua-kk #f)
(test "call/cc re-entry into a middle letrec init" '((2 1 0) 7)
      (letrec ((f (lambda () g))
               (v (call-with-current-continuation
                   (lambda (k) (set! ua-kk k) 0)))
               (g 7))
        (set! ua-r (cons v ua-r))
        (if (< v 2) (ua-kk (+ v 1)) (list ua-r (f)))))
;; A continuation captured in an init, re-entered after the letrec failed
;; on an unassigned read: the later binding is still unassigned.
(define ua-k3 #f)
(define ua-tries 0)
(test "call/cc re-entry after an unassigned read"
      '("Unassigned variable: z" "Unassigned variable: z")
      (let ((msgs '()))
        (let ((m (ua-msg
                  (lambda ()
                    (letrec ((y (call-with-current-continuation
                                 (lambda (k) (set! ua-k3 k) 1)))
                             (w (list y z))
                             (z 2))
                      w)))))
          (set! msgs (cons m msgs))
          (set! ua-tries (+ ua-tries 1))
          (if (< ua-tries 2) (ua-k3 5))
          msgs)))

(test "eval inside a letrec init" "Unassigned variable: y"
      (ua-msg (lambda ()
                (letrec ((x (eval '(+ 1 2) (interaction-environment)))
                         (z (+ x y))
                         (y 1))
                  z))))
(test "never a value: let-bound from an unassigned variable"
      "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x (let ((v y)) v)) (y 1)) x))))
(test "never a value: passed as an argument" "Unassigned variable: y"
      (ua-msg (lambda () (letrec ((x (list 1 y)) (y 1)) x))))

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
