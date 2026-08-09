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
(define (string-search-forward pattern text . args)
  (if (> (length args) 2)
      (error "string-search-forward: expected at most start and end"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length text)))
        (plen (string-length pattern))
        (tlen (string-length text)))
    (if (= plen 0)
        (error "string-search-forward: pattern must be nonempty")
        (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end tlen))
            (error "string-search-forward: invalid search range")
            (let loop ((i start))
              (cond ((> (+ i plen) end) #f)
                    ((string=? pattern (substring text i (+ i plen))) i)
                    (else (loop (+ i 1)))))))))

;; string-search-backward: find last occurrence of pattern in text
(define (string-search-backward pattern text . args)
  (if (> (length args) 2)
      (error "string-search-backward: expected at most start and end"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length text)))
        (plen (string-length pattern))
        (tlen (string-length text)))
    (if (= plen 0)
        (error "string-search-backward: pattern must be nonempty")
        (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end tlen))
            (error "string-search-backward: invalid search range")
            (let loop ((i (- end plen)))
              (cond ((< i start) #f)
                    ((string=? pattern (substring text i (+ i plen)))
                     i)
                    (else (loop (- i 1)))))))))

(define (string-search-all pattern text . args)
  (if (> (length args) 2)
      (error "string-search-all: expected at most start and end"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length text)))
        (plen (string-length pattern))
        (tlen (string-length text)))
    (if (= plen 0)
        (error "string-search-all: pattern must be nonempty")
        (if (or (not (exact-nonnegative-integer? start))
                (not (exact-nonnegative-integer? end))
                (> start end)
                (> end tlen))
            (error "string-search-all: invalid search range")
            (let loop ((i start) (result '()))
              (if (> (+ i plen) end)
                  (reverse result)
                  (loop (+ i 1)
                        (if (string=? pattern (substring text i (+ i plen)))
                            (cons i result)
                            result))))))))

(define (substring? pattern text)
  (if (= (string-length pattern) 0)
      #t
      (not (not (string-search-forward pattern text)))))

(define (string-match-forward string1 string2)
  (let ((limit (min (string-length string1) (string-length string2))))
    (let loop ((i 0))
      (if (and (< i limit)
               (char=? (string-ref string1 i) (string-ref string2 i)))
          (loop (+ i 1))
          i))))

(define (string-match-backward string1 string2)
  (let ((len1 (string-length string1))
        (len2 (string-length string2)))
    (let loop ((i 0))
      (if (and (< i len1)
               (< i len2)
               (char=? (string-ref string1 (- len1 i 1))
                       (string-ref string2 (- len2 i 1))))
          (loop (+ i 1))
          i))))

(define (string-find-next-char string char . args)
  (if (> (length args) 2)
      (error "string-find-next-char: expected at most start and end"))
  (if (not (char? char))
      (error "string-find-next-char: expected a character"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-next-char: invalid search range")
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((char=? (string-ref string i) char) i)
                (else (loop (+ i 1))))))))

(define (string-find-next-char-ci string char . args)
  (if (> (length args) 2)
      (error "string-find-next-char-ci: expected at most start and end"))
  (if (not (char? char))
      (error "string-find-next-char-ci: expected a character"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-next-char-ci: invalid search range")
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((char-ci=? (string-ref string i) char) i)
                (else (loop (+ i 1))))))))

(define (string-find-previous-char string char . args)
  (if (> (length args) 2)
      (error "string-find-previous-char: expected at most start and end"))
  (if (not (char? char))
      (error "string-find-previous-char: expected a character"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-previous-char: invalid search range")
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((char=? (string-ref string i) char) i)
                (else (loop (- i 1))))))))

(define (string-find-previous-char-ci string char . args)
  (if (> (length args) 2)
      (error "string-find-previous-char-ci: expected at most start and end"))
  (if (not (char? char))
      (error "string-find-previous-char-ci: expected a character"))
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-previous-char-ci: invalid search range")
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((char-ci=? (string-ref string i) char) i)
                (else (loop (- i 1))))))))

(define (string-find-next-char-in-set string set . args)
  (if (> (length args) 2)
      (error "string-find-next-char-in-set: expected at most start and end"))
  (%require-char-set "string-find-next-char-in-set" set)
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-next-char-in-set: invalid search range")
        (let loop ((i start))
          (cond ((>= i end) #f)
                ((char-in-set? (string-ref string i) set) i)
                (else (loop (+ i 1))))))))

(define (string-find-previous-char-in-set string set . args)
  (if (> (length args) 2)
      (error "string-find-previous-char-in-set: expected at most start and end"))
  (%require-char-set "string-find-previous-char-in-set" set)
  (let ((start (if (pair? args) (car args) 0))
        (end (if (and (pair? args) (pair? (cdr args)))
                 (cadr args)
                 (string-length string))))
    (if (or (not (exact-nonnegative-integer? start))
            (not (exact-nonnegative-integer? end))
            (> start end)
            (> end (string-length string)))
        (error "string-find-previous-char-in-set: invalid search range")
        (let loop ((i (- end 1)))
          (cond ((< i start) #f)
                ((char-in-set? (string-ref string i) set) i)
                (else (loop (- i 1))))))))

(define (%string-predicate-search who string predicate start end reverse? skip?)
  (if (not (procedure? predicate))
      (error (string-append who ": expected a predicate")))
  (if (or (not (exact-nonnegative-integer? start))
          (not (exact-nonnegative-integer? end))
          (> start end)
          (> end (string-length string)))
      (error (string-append who ": invalid search range")))
  (let ((matches? (lambda (index)
                    (let ((result (predicate (string-ref string index))))
                      (if skip? (not result) result)))))
    (if reverse?
        (let loop ((index (- end 1)))
          (cond ((< index start) #f)
                ((matches? index) index)
                (else (loop (- index 1)))))
        (let loop ((index start))
          (cond ((>= index end) #f)
                ((matches? index) index)
                (else (loop (+ index 1))))))))

(define (string-index string predicate . args)
  (if (> (length args) 2)
      (error "string-index: expected at most start and end"))
  (%string-predicate-search
   "string-index" string predicate
   (if (pair? args) (car args) 0)
   (if (and (pair? args) (pair? (cdr args)))
       (cadr args)
       (string-length string))
   #f #f))

(define (string-index-right string predicate . args)
  (if (> (length args) 2)
      (error "string-index-right: expected at most start and end"))
  (%string-predicate-search
   "string-index-right" string predicate
   (if (pair? args) (car args) 0)
   (if (and (pair? args) (pair? (cdr args)))
       (cadr args)
       (string-length string))
   #t #f))

(define (string-skip string predicate . args)
  (if (> (length args) 2)
      (error "string-skip: expected at most start and end"))
  (%string-predicate-search
   "string-skip" string predicate
   (if (pair? args) (car args) 0)
   (if (and (pair? args) (pair? (cdr args)))
       (cadr args)
       (string-length string))
   #f #t))

(define (string-skip-right string predicate . args)
  (if (> (length args) 2)
      (error "string-skip-right: expected at most start and end"))
  (%string-predicate-search
   "string-skip-right" string predicate
   (if (pair? args) (car args) 0)
   (if (and (pair? args) (pair? (cdr args)))
       (cadr args)
       (string-length string))
   #t #t))

(define (string-take string count)
  (%require-nonnegative-integer "string-take" count)
  (if (> count (string-length string))
      (error "string-take: count exceeds string length"))
  (substring string 0 count))

(define (string-drop string count)
  (%require-nonnegative-integer "string-drop" count)
  (if (> count (string-length string))
      (error "string-drop: count exceeds string length"))
  (substring string count))

(define (string-take-right string count)
  (%require-nonnegative-integer "string-take-right" count)
  (let ((length (string-length string)))
    (if (> count length)
        (error "string-take-right: count exceeds string length"))
    (substring string (- length count) length)))

(define (string-drop-right string count)
  (%require-nonnegative-integer "string-drop-right" count)
  (let ((length (string-length string)))
    (if (> count length)
        (error "string-drop-right: count exceeds string length"))
    (substring string 0 (- length count))))

(define (%string-contains-range who string start end)
  (if (or (not (exact-nonnegative-integer? start))
          (not (exact-nonnegative-integer? end))
          (> start end)
          (> end (string-length string)))
      (error (string-append who ": invalid string range")))
  #t)

(define (%string-contains-args who string1 string2 args)
  (if (not (or (= (length args) 0)
               (= (length args) 2)
               (= (length args) 4)))
      (error (string-append who ": expected zero, two, or four range arguments")))
  (let ((start1 (if (pair? args) (car args) 0))
        (end1 (if (pair? args)
                  (if (pair? (cdr args)) (cadr args) 0)
                  (string-length string1)))
        (start2 (if (and (pair? args) (pair? (cdr args)))
                    (if (pair? (cddr args)) (caddr args) 0)
                    0))
        (end2 (if (and (pair? args) (pair? (cdr args)))
                  (if (pair? (cddr args))
                      (if (pair? (cdddr args)) (cadddr args) 0)
                      (string-length string2))
                  (string-length string2))))
    (%string-contains-range who string1 start1 end1)
    (%string-contains-range who string2 start2 end2)
    (list start1 end1 start2 end2)))

(define (string-contains string1 string2 . args)
  (let* ((ranges (%string-contains-args "string-contains" string1 string2 args))
         (start1 (car ranges))
         (end1 (cadr ranges))
         (start2 (caddr ranges))
         (end2 (cadddr ranges))
         (needle (substring string2 start2 end2)))
    (if (= start2 end2)
        start1
        (let ((found (string-search-forward
                      needle (substring string1 start1 end1))))
          (if found (+ start1 found) #f)))))

(define (string-contains-right string1 string2 . args)
  (let* ((ranges (%string-contains-args "string-contains-right" string1 string2 args))
         (start1 (car ranges))
         (end1 (cadr ranges))
         (start2 (caddr ranges))
         (end2 (cadddr ranges))
         (needle (substring string2 start2 end2)))
    (if (= start2 end2)
        end1
        (let ((found (string-search-backward
                      needle (substring string1 start1 end1))))
          (if found (+ start1 found) #f)))))

(define (string-find-first-index proc string . strings)
  (let ((all (cons string strings)))
    (let loop ((i 0))
      (if (or (null? all)
              (any (lambda (s) (>= i (string-length s))) all))
          #f
          (if (apply proc (map (lambda (s) (string-ref s i)) all))
              i
              (loop (+ i 1)))))))

(define (string-find-last-index proc string . strings)
  (let* ((all (cons string strings))
         (limit (apply min (map string-length all))))
    (let loop ((i (- limit 1)))
      (if (< i 0)
          #f
          (if (apply proc (map (lambda (s) (string-ref s i)) all))
              i
              (loop (- i 1)))))))

;;; ============================================================================
;;; Bitwise helpers (MIT Scheme compatibility)
;;; ============================================================================

(define (%require-bit-index who bit-num)
  (if (not (and (exact? bit-num)
                (integer? bit-num)
                (>= bit-num 0)))
      (error (string-append who
                            ": expected a non-negative exact integer bit position")))
  bit-num)

(define (%require-bit-integer who value)
  (if (not (and (exact? value) (integer? value)))
      (error (string-append who ": expected an exact integer")))
  value)

(define (bit bit-num)
  (arithmetic-shift 1 (%require-bit-index "bit" bit-num)))

(define (bits first-bit last-bit)
  (let* ((first (%require-bit-index "bits" first-bit))
         (last (%require-bit-index "bits" last-bit))
         (low (min first last))
         (high (max first last)))
    (arithmetic-shift (- (arithmetic-shift 1 (+ (- high low) 1)) 1)
                      low)))

(define (set-bit bit-num value)
  (bitwise-ior (%require-bit-integer "set-bit" value)
               (bit bit-num)))

(define (clear-bit bit-num value)
  (bitwise-and (%require-bit-integer "clear-bit" value)
               (bitwise-not (bit bit-num))))

(define (toggle-bit bit-num value)
  (bitwise-xor (%require-bit-integer "toggle-bit" value)
               (bit bit-num)))

(define (bit-set? bit-num val)
  (not (= 0 (bitwise-and (%require-bit-integer "bit-set?" val)
                         (bit bit-num)))))

(define (extract-bit bit-num value)
  (if (bit-set? bit-num (%require-bit-integer "extract-bit" value)) 1 0))

(define (bit-clear? bit-num value)
  (not (bit-set? bit-num (%require-bit-integer "bit-clear?" value))))

(define (first-set-bit value)
  (let ((value (%require-bit-integer "first-set-bit" value)))
    (if (= value 0)
        -1
        (let loop ((bit-num 0))
          (if (bit-set? bit-num value)
              bit-num
              (loop (+ bit-num 1)))))))

(define (integer-length value)
  (let ((value (%require-bit-integer "integer-length" value)))
    (let loop ((value (if (< value 0) (bitwise-not value) value))
               (length 0))
      (if (= value 0)
          length
          (loop (arithmetic-shift value -1) (+ length 1))))))

(define (bitwise-andc1 first second)
  (bitwise-and (bitwise-not (%require-bit-integer "bitwise-andc1" first))
               (%require-bit-integer "bitwise-andc1" second)))

(define (bitwise-andc2 first second)
  (bitwise-and (%require-bit-integer "bitwise-andc2" first)
               (bitwise-not (%require-bit-integer "bitwise-andc2" second))))

(define (bitwise-orc1 first second)
  (bitwise-ior (bitwise-not (%require-bit-integer "bitwise-orc1" first))
               (%require-bit-integer "bitwise-orc1" second)))

(define (bitwise-orc2 first second)
  (bitwise-ior (%require-bit-integer "bitwise-orc2" first)
               (bitwise-not (%require-bit-integer "bitwise-orc2" second))))

(define (bitwise-nand first second)
  (bitwise-not
   (bitwise-and (%require-bit-integer "bitwise-nand" first)
                (%require-bit-integer "bitwise-nand" second))))

(define (bitwise-nor first second)
  (bitwise-not
   (bitwise-ior (%require-bit-integer "bitwise-nor" first)
                (%require-bit-integer "bitwise-nor" second))))

(define (bitwise-eqv . values)
  (if (null? values)
      -1
      (bitwise-not (apply bitwise-xor
                         (map (lambda (value)
                                (%require-bit-integer "bitwise-eqv" value))
                              values)))))

(define (shift-left value amount)
  (arithmetic-shift (%require-bit-integer "shift-left" value)
                    (%require-bit-index "shift-left" amount)))

(define (shift-right value amount)
  (arithmetic-shift (%require-bit-integer "shift-right" value)
                    (- (%require-bit-index "shift-right" amount))))

(define (bit-mask size position)
  (let ((size (%require-bit-index "bit-mask" size))
        (position (%require-bit-index "bit-mask" position)))
    (if (= size 0)
        0
        (arithmetic-shift (- (arithmetic-shift 1 size) 1) position))))

(define (bit-antimask size position)
  (bitwise-not (bit-mask size position)))

;; MIT Scheme compatibility aliases
(define random random-integer)

;; R7RS binary I/O
(define (write-u8 byte . opt-port)
  (if (> (length opt-port) 1)
      (error "write-u8: expected at most one port"))
  (if (or (not (exact-integer? byte)) (< byte 0) (> byte 255))
      (error "write-u8: expected an integer in the range 0 to 255"))
  (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
    (cond
      ((bytevector-output-port-open? port)
       (vector-set! port 1 (cons (bytevector byte) (vector-ref port 1))))
      ((bytevector-output-port? port)
       (error "write-u8: port is closed"))
      ((binary-port? port)
       (write-bytevector (bytevector byte) port))
      (else
       (error "write-u8: not a binary output port")))))

(define (read-u8 . opt-port)
  (if (> (length opt-port) 1)
      (error "read-u8: expected at most one port"))
  (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
    (cond
      ((bytevector-input-port-open? port)
       (let* ((bv (vector-ref port 1))
              (state (vector-ref port 2))
              (pos (car state))
              (end (cdr state)))
         (if (>= pos end)
             (eof-object)
             (begin
               (vector-set! port 2 (cons (+ pos 1) end))
               (bytevector-u8-ref bv pos)))))
      ((bytevector-input-port? port)
       (error "read-u8: port is closed"))
      ((binary-port? port)
       (let ((bv (read-bytevector 1 port)))
         (if (eof-object? bv)
             bv
             (bytevector-u8-ref bv 0))))
      (else
       (error "read-u8: not a binary input port")))))

;; The C text-output primitives understand file and string ports.  Bytevector
;; ports are represented here as vectors, so route implicit and explicit
;; bytevector output through the binary wrappers after rendering objects into
;; a temporary string port.
(let ((primitive-display display)
      (primitive-write write)
      (primitive-write-shared write-shared)
      (primitive-write-simple write-simple)
      (primitive-write-char write-char)
      (primitive-newline newline))
  (define (write-text-to-bytevector-port string port)
    (let ((bytes (string->utf8 string)))
      (let loop ((index 0))
        (if (< index (bytevector-length bytes))
            (begin
              (write-u8 (bytevector-u8-ref bytes index) port)
              (loop (+ index 1)))))))
  (define (render-object-to-bytevector-port writer object port)
    (let ((temporary (open-output-string)))
      (writer object temporary)
      (write-text-to-bytevector-port (get-output-string temporary) port)))
  (set! display
    (lambda (object . opt-port)
      (if (> (length opt-port) 1)
          (error "display: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (render-object-to-bytevector-port primitive-display object port)
            (apply primitive-display (cons object opt-port))))))
  (set! write
    (lambda (object . opt-port)
      (if (> (length opt-port) 1)
          (error "write: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (render-object-to-bytevector-port primitive-write object port)
            (apply primitive-write (cons object opt-port))))))
  (set! write-shared
    (lambda (object . opt-port)
      (if (> (length opt-port) 1)
          (error "write-shared: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (render-object-to-bytevector-port primitive-write-shared object port)
            (apply primitive-write-shared (cons object opt-port))))))
  (set! write-simple
    (lambda (object . opt-port)
      (if (> (length opt-port) 1)
          (error "write-simple: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (render-object-to-bytevector-port primitive-write-simple object port)
            (apply primitive-write-simple (cons object opt-port))))))
  (set! write-char
    (lambda (char . opt-port)
      (if (> (length opt-port) 1)
          (error "write-char: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (write-text-to-bytevector-port (string char) port)
            (apply primitive-write-char (cons char opt-port))))))
  (set! newline
    (lambda opt-port
      (if (> (length opt-port) 1)
          (error "newline: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
        (if (bytevector-output-port? port)
            (write-u8 10 port)
            (apply primitive-newline opt-port))))))

;;; ============================================================================
;;; Binary I/O helpers
;;; ============================================================================

(define (call-with-binary-input-file filename proc)
  (let ((port (open-binary-input-file filename)))
    (call-with-port port proc)))

(define (call-with-binary-output-file filename proc)
  (let ((port (open-binary-output-file filename)))
    (call-with-port port proc)))

;;; ============================================================================
;;; Bytevector ports (in-memory I/O using bytevectors)
;;; ============================================================================

;; Bytevector output port: vector #(tag chunks)
(define (open-output-bytevector)
  (vector 'bvout '()))

(define (bytevector-chunks? chunks)
  (and (list? chunks)
       (let loop ((xs chunks))
         (if (null? xs)
             #t
             (and (bytevector? (car xs))
                  (loop (cdr xs)))))))

(define (bytevector-output-port? x)
  (and (vector? x) (= (vector-length x) 2)
       (or (eq? (vector-ref x 0) 'bvout)
           (eq? (vector-ref x 0) 'bvout-closed))
       (bytevector-chunks? (vector-ref x 1))))

(define (bytevector-output-port-open? x)
  (and (bytevector-output-port? x) (eq? (vector-ref x 0) 'bvout)))

(define (get-output-bytevector port)
  (if (bytevector-output-port? port)
      (apply bytevector-append (reverse (vector-ref port 1)))
      (error "get-output-bytevector: not an output bytevector port")))

;; Bytevector input port: vector #(tag bv (position . end)).  Keep the
;; original bytevector, as MIT's bytevector input ports observe mutations to
;; the source after the port is opened.  The pair stores the half-open range
;; without requiring a copied slice.
(define (open-input-bytevector bv . args)
  (if (not (bytevector? bv))
      (error "open-input-bytevector: expected a bytevector"))
  (if (> (length args) 2)
      (error "open-input-bytevector: too many arguments"))
  (let* ((start (if (pair? args) (car args) 0))
         (rest (if (pair? args) (cdr args) '()))
         (end (if (pair? rest)
                  (car rest)
                  (bytevector-length bv))))
    (%require-nonnegative-integer "open-input-bytevector" start)
    (%require-nonnegative-integer "open-input-bytevector" end)
    (if (or (> start end) (> end (bytevector-length bv)))
        (error "open-input-bytevector: invalid bytevector range"))
    (vector 'bvin bv (cons start end))))

(define (call-with-output-bytevector proc)
  (let ((port (open-output-bytevector)))
    (proc port)
    (get-output-bytevector port)))

(define (bytevector-input-port? x)
  (and (vector? x) (= (vector-length x) 3)
       (or (eq? (vector-ref x 0) 'bvin)
           (eq? (vector-ref x 0) 'bvin-closed))
       (bytevector? (vector-ref x 1))
       (pair? (vector-ref x 2))
       (exact-nonnegative-integer? (car (vector-ref x 2)))
       (exact-nonnegative-integer? (cdr (vector-ref x 2)))
       (<= (car (vector-ref x 2))
           (cdr (vector-ref x 2)))
       (<= (cdr (vector-ref x 2))
           (bytevector-length (vector-ref x 1)))))

(define (bytevector-input-port-open? x)
  (and (bytevector-input-port? x) (eq? (vector-ref x 0) 'bvin)))

;; write-bytevector: write all or part of a bytevector to a port
(define primitive-write-bytevector write-bytevector)

(define (write-bytevector bv . args)
  (if (> (length args) 3)
      (error "write-bytevector: too many arguments"))
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
     (%require-nonnegative-integer "read-bytevector" k)
     (if (> (length args) 1)
         (error "read-bytevector: too many arguments"))
     (let ((port (if (pair? args) (car args) (current-input-port))))
      (cond
        ((bytevector-input-port-open? port)
         (let* ((bv (vector-ref port 1))
                (state (vector-ref port 2))
                (pos (car state))
                (end (cdr state))
                (available (- end pos)))
           (if (= k 0)
               (bytevector)
               (if (<= available 0)
               (eof-object)
               (let* ((n (min k available))
                      (result (bytevector-copy bv pos (+ pos n))))
                 (vector-set! port 2 (cons (+ pos n) end))
                 result)))))
        ((bytevector-input-port? port)
         (error "read-bytevector: port is closed"))
        (else
         (prim-read-bytevector k port)))))))

;; Wrap read-bytevector! to support bytevector input ports.
(let ((prim-read-bytevector! read-bytevector!))
  (set! read-bytevector!
    (lambda (target . args)
      (if (not (bytevector? target))
          (error "read-bytevector!: expected a bytevector"))
      (if (> (length args) 3)
          (error "read-bytevector!: too many arguments"))
      (let* ((port (if (pair? args) (car args) (current-input-port)))
             (rest (if (pair? args) (cdr args) '()))
             (start (if (pair? rest) (car rest) 0))
             (rest2 (if (pair? rest) (cdr rest) '()))
             (end (if (pair? rest2) (car rest2)
                      (bytevector-length target))))
        (if (or (not (exact-nonnegative-integer? start))
                (not (exact-nonnegative-integer? end))
                (> start end)
                (> end (bytevector-length target)))
            (error "read-bytevector!: invalid bytevector range"))
        (cond
          ((bytevector-input-port-open? port)
           (let* ((bv (vector-ref port 1))
                  (state (vector-ref port 2))
                  (pos (car state))
                  (port-end (cdr state))
                  (available (- port-end pos))
                  (count (- end start)))
             (if (= count 0)
                 0
                 (if (<= available 0)
                     (eof-object)
                     (let ((n (min count available)))
                       (let loop ((i 0))
                         (if (= i n)
                             #t
                             (begin
                               (bytevector-u8-set!
                                target (+ start i)
                                (bytevector-u8-ref bv (+ pos i)))
                               (loop (+ i 1)))))
                       (vector-set! port 2 (cons (+ pos n) port-end))
                       n)))))
          ((bytevector-input-port? port)
           (error "read-bytevector!: port is closed"))
          (else
           (apply prim-read-bytevector! (cons target args))))))))

;; peek-u8/u8-ready? are C primitives that only understand real ports;
;; wrap them the same way read-u8/read-bytevector are wrapped above so
;; they also accept the vector-based bytevector input ports.
(let ((primitive-peek-u8 peek-u8)
      (primitive-u8-ready? u8-ready?))
  (set! peek-u8
    (lambda opt-port
      (if (> (length opt-port) 1)
          (error "peek-u8: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
        (cond
          ((bytevector-input-port-open? port)
           (let* ((bv (vector-ref port 1))
                  (state (vector-ref port 2))
                  (pos (car state))
                  (end (cdr state)))
             (if (>= pos end)
                 (eof-object)
                 (bytevector-u8-ref bv pos))))
          ((bytevector-input-port? port)
           (error "peek-u8: port is closed"))
          ((not (binary-port? port))
           (error "peek-u8: not a binary input port"))
          (else
           (apply primitive-peek-u8 opt-port))))))
  (set! u8-ready?
    (lambda opt-port
      (if (> (length opt-port) 1)
          (error "u8-ready?: expected at most one port"))
      (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
        (cond
          ((bytevector-input-port-open? port) #t)
          ((bytevector-input-port? port)
           (error "u8-ready?: port is closed"))
          ((not (binary-port? port))
           (error "u8-ready?: not a binary input port"))
          (else
           (apply primitive-u8-ready? opt-port)))))))

(let ((primitive-input-port? input-port?)
      (primitive-output-port? output-port?)
      (primitive-input-port-open? input-port-open?)
      (primitive-output-port-open? output-port-open?)
      (primitive-textual-port? textual-port?)
      (primitive-binary-port? binary-port?)
      (primitive-close-input-port close-input-port)
      (primitive-close-output-port close-output-port)
      (primitive-flush-output-port flush-output-port))
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
  (set! flush-output-port
    (lambda opt-port
      (if (> (length opt-port) 1)
          (error "flush-output-port: expected at most one port"))
      (let ((port (if (pair? opt-port)
                      (car opt-port)
                      (current-output-port))))
        (cond
          ((bytevector-output-port-open? port) #f)
          ((bytevector-output-port? port)
           (error "flush-output-port: port is closed"))
          (else
           (apply primitive-flush-output-port opt-port))))))
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
          (if (bytevector-input-port? port)
              #f
              (primitive-close-input-port port)))))
  (set! close-output-port
    (lambda (port)
      (if (bytevector-output-port-open? port)
          (vector-set! port 0 'bvout-closed)
          (if (bytevector-output-port? port)
              #f
              (primitive-close-output-port port))))))

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
;; The default handler prints and returns. It deliberately does not exit:
;; unhandled errors propagate as TOK_ERROR to the C boundary, which restores
;; the baseline exception state (see eval_expr in main.c), so an interactive
;; REPL survives an unhandled error instead of losing the session.
(define (*default-exception-handler* exn)
  (display "Unhandled exception: ")
  (display exn)
  (newline)
  (set! *wind-stack* '())
  #f)
(define *current-exception-handler* *default-exception-handler*)

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

;; raise dispatches through raise-now (engine machinery) with a pinned
;; return marker: a default-handler return IS the designed unhandled path
;; (already reported), a user-handler return is the R7RS violation, and a
;; handler that jumps to a continuation (guard) resumes normally.
(define (raise obj)
  (raise-now obj))

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
  (if (and (exact? k) (integer? k) (>= k 0))
      k
      (error (string-append who ": expected nonnegative integer"))))

(define (%require-exact-integer who k)
  (if (and (exact? k) (integer? k))
      k
      (error (string-append who ": expected exact integer"))))

(define (%require-procedure who proc)
  (if (procedure? proc)
      proc
      (error (string-append who ": expected procedure"))))

; Like %require-proper-list, but allows a non-nil final cdr (an improper/
; dotted list), as several R7RS/SRFI-1 procedures explicitly permit.
; Still rejects circular lists (Floyd's cycle detection) so callers can't
; hang walking one.
(define (%require-finite-list who lst)
  ; Accept proper and dotted lists, but do not mistake an arbitrary atom
  ; for a zero-element list.  The latter used to let list-copy/take-right/
  ; drop-right silently accept values such as 42.
  (if (and (not (null? lst)) (not (pair? lst)))
      (error (string-append who ": expected finite list"))
      (let loop ((slow lst) (fast lst))
        (cond ((not (pair? fast)) lst)
              ((not (pair? (cdr fast))) lst)
              (else
               (let ((fast2 (cddr fast)) (slow2 (cdr slow)))
                 (if (eq? fast2 slow2)
                     (error (string-append who ": circular list"))
                     (loop slow2 fast2))))))))

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
  (%require-proper-list "list-ref" lst)
  (%require-nonnegative-integer "list-ref" k)
  (let loop ((lst lst) (k k))
    (cond ((not (pair? lst)) (error "list-ref: index out of bounds"))
          ((= k 0) (car lst))
          (else (loop (cdr lst) (- k 1))))))

(define (list-tail lst k)
  (%require-proper-list "list-tail" lst)
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
  (if (> (length maybe-compare) 1)
      (error "member: too many arguments"))
  (if (null? lst)
      #f
      (begin
        (%require-proper-list "member" lst)
        (let ((compare (%require-procedure
                        "member"
                        (if (null? maybe-compare) equal? (car maybe-compare)))))
          (let loop ((lst lst))
            (cond ((null? lst) #f)
                  ((compare obj (car lst)) lst)
                  (else (loop (cdr lst)))))))))

(define (member-procedure predicate)
  (lambda (obj lst)
    (%require-proper-list "member-procedure" lst)
    (let loop ((lst lst))
      (cond ((null? lst) #f)
            ((predicate obj (car lst)) lst)
            (else (loop (cdr lst)))))))

; Association list functions
(define (alist? object)
  (and (proper-list? object)
       (let loop ((alist object))
         (or (null? alist)
             (and (pair? (car alist))
                  (loop (cdr alist)))))))

(define (assq obj alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
          ((not (pair? alist)) (error "assq: improper association list"))
          ((eq? obj (caar alist)) (car alist))
          (else (loop (cdr alist))))))

(define (assv obj alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
          ((not (pair? alist)) (error "assv: improper association list"))
          ((eqv? obj (caar alist)) (car alist))
          (else (loop (cdr alist))))))

(define (assoc obj alist . maybe-compare)
  (if (> (length maybe-compare) 1)
      (error "assoc: too many arguments"))
  (if (null? alist)
      #f
      (begin
        (let ((compare (%require-procedure
                        "assoc"
                        (if (null? maybe-compare) equal? (car maybe-compare)))))
          (let loop ((alist alist))
            (cond ((null? alist) #f)
                  ((not (pair? alist))
                   (error "assoc: improper association list"))
                  ((compare obj (caar alist)) (car alist))
                  (else (loop (cdr alist)))))))))

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
        ; Use the reversed comparison so NaNs and signed zeros follow the
        ; same operand-selection behavior as MIT/GNU Scheme: keep x unless
        ; the accumulated minimum is strictly greater than it.
        (if (> x m) m x))))

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
  ; MIT does not inspect the procedure or companion arguments when the
  ; first list is already empty.  Keep that short-circuit before validating
  ; circular/proper companion lists, while retaining the checks for every
  ; call that will actually traverse the input.
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "map" (cons lst lsts))
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
                    (let ((new-cell
                           (cons (apply proc (car lst) (map car lsts))
                                 '())))
                      (set-cdr! tail new-cell)
                      (loop (cdr lst) (map cdr lsts) new-cell)))))))))

; SRFI-1's map-in-order is the deterministic counterpart to map.  This
; implementation already evaluates map's procedure calls left-to-right.
(define map-in-order map)

(define (for-each proc lst . lsts)
  ; As with map, an empty first list means no procedure or companion is
  ; inspected.  This is observable with invalid unused arguments in MIT.
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "for-each" (cons lst lsts))
        (if (null? lsts)
            ; Single list case.  Keep the traversal in tail position so a
            ; large proper list does not consume one Scheme stack frame per
            ; element.
            (let loop ((lst lst))
              (if (pair? lst)
                  (begin
                    (proc (car lst))
                    (loop (cdr lst)))))
            ; Multiple lists case.  Stop at the first finite list, while
            ; allowing circular companions as SRFI-1 does.
            (let loop ((lst lst) (lsts lsts))
              (if (and (pair? lst) (not (any null? lsts)))
                  (begin
                    (apply proc (car lst) (map car lsts))
                    (loop (cdr lst) (map cdr lsts)))))))))

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
    (call-with-port port proc)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (call-with-port port proc)))

; with-input-from-file temporarily redirects current-input-port
(define (with-input-from-file filename thunk)
  (let* ((new-port (open-input-file filename))
         (old-port (current-input-port))
         (returned? #f))
    (dynamic-wind
      (lambda () (set-current-input-port! new-port))
      (lambda ()
        (call-with-values
          thunk
          (lambda results
            (set! returned? #t)
            (apply values results))))
      (lambda ()
        (set-current-input-port! old-port)
        (if returned?
            (close-input-port new-port))))))

; with-output-to-file temporarily redirects current-output-port
(define (with-output-to-file filename thunk)
  (let* ((new-port (open-output-file filename))
         (old-port (current-output-port))
         (returned? #f))
    (dynamic-wind
      (lambda () (set-current-output-port! new-port))
      (lambda ()
        (call-with-values
          thunk
          (lambda results
            (set! returned? #t)
            (apply values results))))
      (lambda ()
        (set-current-output-port! old-port)
        (if returned?
            (close-output-port new-port))))))

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

; Return procedures that remove matching elements from a list.
(define (list-deletor pred)
  (lambda (lst) (remove pred lst)))

(define (list-deletor! pred)
  (lambda (lst) (remove! pred lst)))

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
  (if (null? lst)
      #f
      (begin
        (%require-circular-lists "any" (cons lst more-lists))
        (let loop ((lsts (cons lst more-lists)))
          (if (any-null? lsts)
              #f
              (or (apply pred (map car lsts))
                  (loop (map cdr lsts))))))))

; every - return the predicate's value for the last element(s) (not a bare
; #t), stopping at the shortest of any number of lists; #t if all satisfy
; (or any list is empty from the start) (SRFI-1)
(define (every pred lst . more-lists)
  (if (null? lst)
      #t
      (begin
        (%require-circular-lists "every" (cons lst more-lists))
        (let loop ((lsts (cons lst more-lists)) (last-result #t))
          (if (any-null? lsts)
              last-result
              (let ((r (apply pred (map car lsts))))
                (if r (loop (map cdr lsts) r) #f)))))))

(define (any-null? lsts)
  (if (null? lsts) #f (or (null? (car lsts)) (any-null? (cdr lsts)))))

; count - count elements satisfying predicate
(define (count pred lst . more-lists)
  (if (null? lst)
      0
      (begin
        (%require-circular-lists "count" (cons lst more-lists))
        (let loop ((lists (cons lst more-lists)) (n 0))
          (if (any null? lists)
              n
              (loop (map cdr lists)
                    (if (apply pred (map car lists))
                        (+ n 1)
                        n)))))))

; fold - left-associative fold over one or more lists (SRFI-1).
(define (fold proc init lst . more-lists)
  (%require-circular-lists "fold" (cons lst more-lists))
  (let loop ((lists (cons lst more-lists)) (acc init))
    (if (any null? lists)
        acc
        (loop (map cdr lists)
              (apply proc (append (map car lists) (list acc)))))))

; fold-right - right-associative fold over one or more lists (SRFI-1).
(define (fold-right proc init lst . more-lists)
  (%require-circular-lists "fold-right" (cons lst more-lists))
  (let loop ((lists (cons lst more-lists)))
    (if (any null? lists)
        init
        (apply proc
               (append (map car lists)
                       (list (loop (map cdr lists))))))))

; reduce - like fold but uses first element as initial value; returns
; ridentity for an empty list (SRFI-1 signature: (reduce f ridentity list))
(define (reduce proc ridentity lst)
  (%require-proper-list "reduce" lst)
  (if (null? lst)
      ridentity
      (fold proc (car lst) (cdr lst))))

; take - return first n elements of list (SRFI-1/R7RS-large signature:
; (take list k), matching MIT and every sibling function in this file)
(define (take lst n)
  (%require-nonnegative-integer "take" n)
  (let loop ((n n) (lst lst))
    ; The explicit count bounds circular-list traversal, but a finite list
    ; must still contain every requested element.
    (if (= n 0)
        '()
        (if (pair? lst)
            (cons (car lst) (loop (- n 1) (cdr lst)))
            (error "take: list is shorter than requested count")))))

; drop - return list without first n elements (SRFI-1/R7RS-large
; signature: (drop list k))
(define (drop lst n)
  (%require-nonnegative-integer "drop" n)
  (let loop ((n n) (lst lst))
    ; Preserve a dotted tail once the requested prefix has been skipped, but
    ; reject a finite list that ends before all requested elements are gone.
    (if (= n 0)
        lst
        (if (pair? lst)
            (loop (- n 1) (cdr lst))
            (error "drop: list is shorter than requested count")))))

; partition - split list into two lists based on predicate
(define (partition pred lst)
  (%require-proper-list "partition" lst)
  (let loop ((lst lst) (yes '()) (no '()))
    (cond ((null? lst) (values (reverse yes) (reverse no)))
          ((pred (car lst)) (loop (cdr lst) (cons (car lst) yes) no))
          (else (loop (cdr lst) yes (cons (car lst) no))))))

; zip - combine corresponding elements of one or more lists, stopping at the
; shortest list (SRFI-1).
(define (zip . lists)
  (if (null? lists)
      (error "zip: expected at least one list"))
  (%require-proper-lists "zip" lists)
  (let loop ((lists lists) (result '()))
    (if (any null? lists)
        (reverse result)
        (loop (map cdr lists)
              (cons (map car lists) result)))))

; flatten - flatten nested list structure
(define (flatten lst)
  (if (pair? lst)
      (%require-proper-list "flatten" lst))
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

; last - return last element of list
(define (last lst)
  (%require-proper-list "last" lst)
  (if (null? lst)
      (error "last: expected non-empty list"))
  (car (last-pair lst)))

; iota - generate a list of count numbers: start, start+step, ...
; (SRFI-1/R7RS-large signature: (iota count [start [step]]))
(define (iota count . args)
  (%require-nonnegative-integer "iota" count)
  (if (> (length args) 2)
      (error "iota: too many arguments"))
  (let ((start (if (pair? args) (car args) 0))
        (step (if (and (pair? args) (pair? (cdr args))) (cadr args) 1)))
    (let loop ((i (- count 1)) (acc '()))
      (if (< i 0)
          acc
          (loop (- i 1) (cons (+ start (* i step)) acc))))))

; range - generate list of integers [start, end)
(define (range start end)
  (%require-exact-integer "range" start)
  (%require-exact-integer "range" end)
  (let loop ((i (- end 1)) (acc '()))
    (if (< i start)
        acc
        (loop (- i 1) (cons i acc)))))

;;; ============================================================================
;;; Additional SRFI-1 Constructors
;;; ============================================================================

; circular-list - create a cyclic list.  The empty case is accepted as the
; MIT/GNU extension, where it is simply the empty list.
(define (circular-list . objects)
  (if (null? objects)
      '()
      (let loop ((tail objects))
        (if (null? (cdr tail))
            (begin
              (set-cdr! tail objects)
              objects)
            (loop (cdr tail))))))

; make-circular-list - create a cyclic list of length k.
(define (make-circular-list k . fill)
  (%require-nonnegative-integer "make-circular-list" k)
  (if (> (length fill) 1)
      (error "make-circular-list: too many arguments"))
  (if (= k 0)
      '()
      (apply circular-list
             (make-list k (if (null? fill) #f (car fill))))))

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
  (if (> (length fill) 1)
      (error "make-list: too many arguments"))
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
  (if (not (pair? lst))
      lst
      (begin
        (%require-finite-list "list-copy" lst)
        (let loop ((lst lst))
          (if (pair? lst)
              (cons (car lst) (loop (cdr lst)))
              lst)))))

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

; circular-list? - true only for a non-empty list whose cdr chain cycles.
(define (circular-list? x)
  (and (pair? x)
       (not (proper-list? x))
       (not (dotted-list? x))))

; Validate a family of proper or circular lists.  SRFI-1 permits circular
; inputs for multi-list traversals, but requires at least one finite list so
; the traversal has a defined stopping point.
(define (%require-circular-lists who lists)
  (%require-proper-list who lists)
  (if (null? lists)
      (error (string-append who ": expected at least one list")))
  (let loop ((lists lists) (all-circular? #t))
    (if (null? lists)
        (if all-circular?
            (error (string-append who
                                  ": at least one list must be finite"))
            #t)
        (let ((lst (car lists)))
          (cond ((proper-list? lst)
                 (loop (cdr lists) #f))
                ((circular-list? lst)
                 (loop (cdr lists) all-circular?))
                (else
                 (error (string-append who
                                       ": expected proper or circular list"))))))))

; length+ - length for a proper list, or #f for a circular list.
(define (length+ x)
  (cond ((proper-list? x) (length x))
        ((circular-list? x) #f)
        (else (error "length+: expected proper or circular list"))))

; list= - compare lists element-wise using elt=
(define (list= elt= . lists)
  (or (null? lists)
      (null? (cdr lists))
      (begin
        (%require-proper-lists "list=" lists)
        (let loop ((lists lists))
          (or (null? (cdr lists))
              (let ((a (car lists)) (b (cadr lists)))
                (and (let cmp ((a a) (b b))
                       (cond ((null? a) (null? b))
                             ((null? b) #f)
                             ((elt= (car a) (car b))
                              (cmp (cdr a) (cdr b)))
                             (else #f)))
                     (loop (cdr lists)))))))))

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

; Count the pairs in a list-like object.  Unlike `length`, this accepts
; dotted and circular lists; for a circular list, count each pair through
; the first repeated pair exactly once.  Other objects count as zero.
(define (count-pairs object)
  (define (finite-count lst)
    (let loop ((lst lst) (n 0))
      (if (pair? lst)
          (loop (cdr lst) (+ n 1))
          n)))
  (define (cycle-length entry)
    (let loop ((lst (cdr entry)) (n 1))
      (if (eq? lst entry)
          n
          (loop (cdr lst) (+ n 1)))))
  (let detect ((slow object) (fast object))
    (cond
     ((not (pair? fast)) (finite-count object))
     ((not (pair? (cdr fast))) (finite-count object))
     (else
      (let ((slow2 (cdr slow))
            (fast2 (cdr (cdr fast))))
        (if (eq? slow2 fast2)
            (let find-entry ((a object) (b slow2) (prefix 0))
              (if (eq? a b)
                  (+ prefix (cycle-length a))
                  (find-entry (cdr a) (cdr b) (+ prefix 1))))
            (detect slow2 fast2)))))))

; Internal users need the same cycle-safe semantics.
(define (%pair-count lst)
  (count-pairs lst))

; take-right - return last n elements. SRFI-1/MIT permit a possibly
; improper (dotted) list, returning the final non-nil cdr along with the
; last n elements if n reaches all the way to it.
(define (take-right lst n)
  (%require-finite-list "take-right" lst)
  (%require-nonnegative-integer "take-right" n)
  (let ((len (%pair-count lst)))
    (if (> n len)
        (error "take-right: list is shorter than requested count")
        (if (= n len)
            lst
            (drop lst (- len n))))))

; drop-right - return all but last n elements
(define (drop-right lst n)
  (%require-finite-list "drop-right" lst)
  (%require-nonnegative-integer "drop-right" n)
  (let ((len (%pair-count lst)))
    (if (> n len)
        (error "drop-right: list is shorter than requested count")
        (if (= n len)
            '()
            (take lst (- len n))))))

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
  (%require-proper-lists "unzip1" lists)
  (map car lists))

; unzip2 - extract first two elements from list of lists
(define (unzip2 lists)
  (%require-proper-lists "unzip2" lists)
  (values (map car lists) (map cadr lists)))

; unzip3 - extract first three elements
(define (unzip3 lists)
  (%require-proper-lists "unzip3" lists)
  (values (map car lists) (map cadr lists) (map caddr lists)))

; unzip4 - extract first four elements
(define (unzip4 lists)
  (%require-proper-lists "unzip4" lists)
  (values (map car lists) (map cadr lists) (map caddr lists) (map cadddr lists)))

; unzip5 - extract first five elements
(define (unzip5 lists)
  (%require-proper-lists "unzip5" lists)
  (values (map car lists) (map cadr lists) (map caddr lists)
          (map cadddr lists) (map fifth lists)))

;;; ============================================================================
;;; Additional SRFI-1 Fold/Unfold/Map
;;; ============================================================================

; pair-fold - like fold but proc receives pairs, not elements.
(define (pair-fold proc init lst . more-lists)
  (if (null? lst)
      init
      (begin
        (%require-circular-lists "pair-fold" (cons lst more-lists))
        (let loop ((lists (cons lst more-lists)) (acc init))
          (if (any null? lists)
              acc
              (loop (map cdr lists)
                    (apply proc (append lists (list acc)))))))))

; pair-fold-right - like fold-right but proc receives pairs.
(define (pair-fold-right proc init lst . more-lists)
  (if (null? lst)
      init
      (begin
        (%require-circular-lists "pair-fold-right" (cons lst more-lists))
        (let loop ((lists (cons lst more-lists)))
          (if (any null? lists)
              init
              (apply proc
                     (append lists
                             (list (loop (map cdr lists))))))))))

; reduce-right - like reduce but right-associative
(define (reduce-right proc init lst)
  (%require-proper-list "reduce-right" lst)
  (if (null? lst)
      init
      (let loop ((rest lst))
        (if (null? (cdr rest))
            (car rest)
            (proc (car rest) (loop (cdr rest)))))))

; unfold - generate list from seed using p, f, g
(define (unfold p f g seed . maybe-tail-gen)
  (if (> (length maybe-tail-gen) 1)
      (error "unfold: too many arguments"))
  (let ((tail-gen (if (null? maybe-tail-gen)
                      (lambda (x) '())
                      (car maybe-tail-gen))))
    (let loop ((seed seed))
      (if (p seed)
          (tail-gen seed)
          (cons (f seed) (loop (g seed)))))))

; unfold-right - like unfold but builds list right-to-left
(define (unfold-right p f g seed . maybe-tail)
  (if (> (length maybe-tail) 1)
      (error "unfold-right: too many arguments"))
  (let ((tail (if (null? maybe-tail) '() (car maybe-tail))))
    (let loop ((seed seed) (acc tail))
      (if (p seed)
          acc
          (loop (g seed) (cons (f seed) acc))))))

; append-map - map then append results
(define (append-map proc lst . lsts)
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "append-map" (cons lst lsts))
        (apply append (apply map proc lst lsts)))))

; append-map! - map then destructively append the resulting lists.
(define (append-map! proc lst . lsts)
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "append-map!" (cons lst lsts))
        (apply append! (apply map proc lst lsts)))))

; filter-map - map and filter in one pass over one or more lists.
(define (filter-map proc lst . more-lists)
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "filter-map" (cons lst more-lists))
        (let loop ((lists (cons lst more-lists)) (acc '()))
          (if (any null? lists)
              (reverse acc)
              (let ((result (apply proc (map car lists))))
                (loop (map cdr lists)
                      (if result (cons result acc) acc))))))))

; pair-for-each - like for-each but proc receives pairs.  As with the other
; SRFI-1 multi-list traversals, stop at the shortest list and permit circular
; companions as long as at least one input list is finite.
(define (pair-for-each proc lst . more-lists)
  (if (null? lst)
      '()
      (begin
        (%require-circular-lists "pair-for-each" (cons lst more-lists))
        (let loop ((lists (cons lst more-lists)))
          (if (not (any-null? lists))
              (let ((next (map cdr lists)))
                ; Capture successors before the callback: SRFI-1 permits the
                ; callback to mutate the pairs it receives, without changing
                ; the sequence of pairs that pair-for-each visits.
                (apply proc lists)
                (loop next)))))))

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
  (if (> (length maybe-eq) 1)
      (error "delete: too many arguments"))
  (if (null? lst)
      '()
      (let ((eq (%require-procedure
                 "delete"
                 (if (null? maybe-eq) equal? (car maybe-eq)))))
        (filter (lambda (y) (not (eq x y))) lst))))

; delete-duplicates - remove duplicate elements
(define (delete-duplicates lst . maybe-eq)
  (if (> (length maybe-eq) 1)
      (error "delete-duplicates: too many arguments"))
  (if (null? lst)
      '()
      (begin
        (%require-proper-list "delete-duplicates" lst)
        (let ((eq (%require-procedure
                   "delete-duplicates"
                   (if (null? maybe-eq) equal? (car maybe-eq)))))
          (let loop ((lst lst) (seen '()))
            (cond ((null? lst) (reverse seen))
                  ((any (lambda (y) (eq (car lst) y)) seen)
                   (loop (cdr lst) seen))
                  (else (loop (cdr lst) (cons (car lst) seen)))))))))

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
  (if (> (length maybe-eq) 1)
      (error "alist-delete: too many arguments"))
  (if (null? alist)
      '()
      (let ((eq (%require-procedure
                 "alist-delete"
                 (if (null? maybe-eq) equal? (car maybe-eq)))))
        (filter (lambda (pair) (not (eq key (car pair)))) alist))))

;;; SRFI-1 set operations on lists

(define (%lset-member? elt lst elt=)
  (let loop ((lst lst))
    (cond ((null? lst) #f)
          ((elt= (car lst) elt) #t)
          (else (loop (cdr lst))))))

(define (%lset-subset? left right elt=)
  (let loop ((left left))
    (or (null? left)
        (and (%lset-member? (car left) right elt=)
             (loop (cdr left))))))

(define (%lset-require-lists who lists)
  (%require-proper-list who lists)
  (for-each (lambda (lst) (%require-proper-list who lst)) lists)
  lists)

(define (lset<= elt= . lists)
  (%lset-require-lists "lset<=" lists)
  (if (null? lists)
      #t
      (let loop ((lists lists))
        (or (null? (cdr lists))
            (and (%lset-subset? (car lists) (cadr lists) elt=)
                 (loop (cdr lists)))))))

(define (lset= elt= . lists)
  (%lset-require-lists "lset=" lists)
  (if (null? lists)
      #t
      (let loop ((lists lists))
        (or (null? (cdr lists))
            (and (%lset-subset? (car lists) (cadr lists) elt=)
                 (%lset-subset? (cadr lists) (car lists) elt=)
                 (loop (cdr lists)))))))

(define (lset-adjoin elt= list . elts)
  (%require-proper-list "lset-adjoin" list)
  (let loop ((elts elts) (result list))
    (if (null? elts)
        result
        (let ((elt (car elts)))
          (loop (cdr elts)
                (if (%lset-member? elt result elt=)
                    result
                    (cons elt result)))))))

(define (%lset-union2 elt= first second)
  (let loop ((rest second) (result (list-copy first)))
    (if (null? rest)
        result
        (let ((elt (car rest)))
          (loop (cdr rest)
                (if (%lset-member? elt result elt=)
                    result
                    (cons elt result)))))))

(define (lset-union elt= . lists)
  (%lset-require-lists "lset-union" lists)
  (if (null? lists)
      '()
      (let loop ((lists (cdr lists)) (result (list-copy (car lists))))
        (if (null? lists)
            result
            (loop (cdr lists) (%lset-union2 elt= result (car lists)))))))

(define (lset-intersection elt= first . rest)
  (%require-proper-list "lset-intersection" first)
  (%lset-require-lists "lset-intersection" rest)
  (filter
   (lambda (elt)
     (let loop ((lists rest))
       (or (null? lists)
           (and (%lset-member? elt (car lists) elt=)
                (loop (cdr lists))))))
   first))

(define (lset-difference elt= first . rest)
  (%require-proper-list "lset-difference" first)
  (%lset-require-lists "lset-difference" rest)
  (filter
   (lambda (elt)
     (let loop ((lists rest))
       (or (null? lists)
           (and (not (%lset-member? elt (car lists) elt=))
                (loop (cdr lists))))))
   first))

(define (lset-xor elt= . lists)
  (%lset-require-lists "lset-xor" lists)
  (let loop ((lists lists) (result '()))
    (if (null? lists)
        result
        (if (null? result)
            (loop (cdr lists) (list-copy (car lists)))
            (let ((left (lset-difference elt= result (car lists)))
                  (right (lset-difference elt= (car lists) result)))
              (loop (cdr lists) (append (reverse left) right)))))))

(define (lset-diff+intersection elt= first . rest)
  (%require-proper-list "lset-diff+intersection" first)
  (%lset-require-lists "lset-diff+intersection" rest)
  (let ((others (apply lset-union elt= rest)))
    (values (lset-difference elt= first others)
            (lset-intersection elt= first others))))

; SRFI-1 linear-update procedures are permitted to be pure implementations.
(define lset-union! lset-union)
(define lset-intersection! lset-intersection)
(define lset-difference! lset-difference)
(define lset-xor! lset-xor)
(define lset-diff+intersection! lset-diff+intersection)

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
; finite list; a dotted prefix is permitted and its terminal tail is
; replaced by the next argument, matching MIT's destructive behavior.  The
; last argument may be any object.
(define (append! . lists)
  (if (pair? lists)
      (let validate ((rest lists))
        (if (pair? (cdr rest))
            (begin
              (%require-finite-list "append!" (car rest))
              (validate (cdr rest)))))
      lists)
  (if (null? lists)
      '()
      (let loop ((result '()) (lists lists))
        (cond ((null? lists) result)
              ((null? (car lists)) (loop result (cdr lists)))
              ((null? result) (loop (car lists) (cdr lists)))
              (else
               (let attach ((tail result))
                 (if (pair? (cdr tail))
                     (attach (cdr tail))
                     (set-cdr! tail (car lists))))
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
  (if (null? lst)
      lst
      (begin
        (%require-circular-lists "map!" (cons lst lsts))
        (if (null? lsts)
            ; Single list case
            (let loop ((pair lst))
              (if (null? pair)
                  lst
                  (begin
                    (set-car! pair (proc (car pair)))
                    (loop (cdr pair)))))
            ; Multiple lists case
            (begin
              ; SRFI-1 requires every companion list to be at least as long
              ; as the first list.  Validate this before changing the target
              ; list; circular companions naturally satisfy the requirement.
              (let validate ((pair lst) (others lsts))
                (if (null? pair)
                    #t
                    (if (any null? others)
                        (error "map!: companion list is shorter than target")
                        (validate (cdr pair) (map cdr others)))))
              (let loop ((pair lst) (others lsts))
                (if (null? pair)
                    lst
                    (begin
                      (set-car! pair (apply proc (car pair) (map car others)))
                      (loop (cdr pair) (map cdr others))))))))))

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
  (if (> (length maybe-eq) 1)
      (error "delete!: too many arguments"))
  (if (null? lst)
      '()
      (let ((eq (%require-procedure
                 "delete!"
                 (if (null? maybe-eq) equal? (car maybe-eq)))))
        (filter! (lambda (y) (not (eq x y))) lst))))

; delete-duplicates! - destructive delete-duplicates
(define (delete-duplicates! lst . maybe-eq)
  (if (> (length maybe-eq) 1)
      (error "delete-duplicates!: too many arguments"))
  (if (null? lst)
      '()
      (begin
        (%require-proper-list "delete-duplicates!" lst)
        (let ((eq (%require-procedure
                   "delete-duplicates!"
                   (if (null? maybe-eq) equal? (car maybe-eq)))))
          (let loop ((curr lst))
            (if (null? curr)
                lst
                (begin
                  (set-cdr! curr (delete! (car curr) (cdr curr) eq))
                  (loop (cdr curr)))))))))

; alist-delete! - destructive alist-delete
(define (alist-delete! key alist . maybe-eq)
  (if (> (length maybe-eq) 1)
      (error "alist-delete!: too many arguments"))
  (if (null? alist)
      '()
      (begin
        (%require-proper-list "alist-delete!" alist)
        (let ((eq (%require-procedure
                   "alist-delete!"
                   (if (null? maybe-eq) equal? (car maybe-eq)))))
          (filter! (lambda (pair) (not (eq key (car pair)))) alist)))))

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
    (if (circular-list? lst)
        (error "dynamic-wind: circular wind stack")
        (let loop ((l lst) (n 0))
          (if (null? l) n (loop (cdr l) (+ n 1))))))
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

; exit runs dynamic-wind after-thunks before terminating (R7RS 6.11: exit
; behaves like a non-local exit, so before/after thunks must run).
; emergency-exit is the escape hatch that skips them.
(define (exit . args)
  (do-wind *wind-stack* '())
  (if (null? args)
      (primitive-exit 0)
      (apply primitive-exit args)))

; dynamic-wind: establish before/after thunks around body
(define dynamic-wind
  (lambda (before body after)
    (before)
    (set! *wind-stack* (cons (cons before after) *wind-stack*))
    (call-with-values
      body
      (lambda results
        (set! *wind-stack* (cdr *wind-stack*))
        (after)
        (apply values results)))))

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
  (if (> (length maybe-converter) 1)
      (error "make-parameter: too many arguments"))
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

; Tail-recursive merge.  The natural (cons x (loop ...)) formulation recurses
; once per merged element, so the final merge of a 100000-element sort blew
; the VM call stack outright.  Accumulating reversed and unwinding at the end
; runs in constant stack depth.  Taking from `right` only when it is strictly
; less than `left` is what makes the sort stable; preserve that comparison.
; Private: skips the argument validation, so merge-sort does not re-walk both
; halves at every node of the merge tree (which was O(n log n) extra walks).
; `unwind` is an internal define on purpose, and it is the faster choice, not
; merely the tidier one: as a top-level helper it would be resolved through
; the global frame - a ~950-entry alist - on every one of the ~2n calls
; merge-sort makes, which measured 3.9s against 2.4s for a 30000-element sort.
; The public append-reverse is not usable here either, since it would
; revalidate a list this procedure just built itself, at every node.
(define (%merge-nocheck less? left right)
  (define (unwind acc tail)
    (if (null? acc)
        tail
        (unwind (cdr acc) (cons (car acc) tail))))
  (let loop ((left left) (right right) (acc '()))
    (cond ((null? left) (unwind acc right))
          ((null? right) (unwind acc left))
          ((less? (car right) (car left))
           (loop left (cdr right) (cons (car right) acc)))
          (else
           (loop (cdr left) right (cons (car left) acc))))))

(define (merge less? left right)
  (%require-proper-list "merge" left)
  (%require-proper-list "merge" right)
  (%merge-nocheck less? left right))

(define (list-sort less? lst . maybe-key)
  (if (not (procedure? less?))
      (error "list-sort: expected comparison procedure"))
  (%require-proper-list "list-sort" lst)
  (if (> (length maybe-key) 1)
      (error "list-sort: too many arguments"))
  (let ((key (if (null? maybe-key) (lambda (x) x) (car maybe-key))))
    (if (not (procedure? key))
        (error "list-sort: expected key procedure"))
    ; With no key argument the wrapper would be (less? (identity a)
    ; (identity b)) - three procedure calls per comparison instead of one,
    ; on the innermost loop of the sort. Use less? directly instead.
    (let ((compare (if (null? maybe-key)
                       less?
                       (lambda (a b) (less? (key a) (key b))))))
      (let sort-list ((xs lst))
        ; (cdr (cdr fast)) rather than cddr: cddr is a Scheme-level define,
        ; not a C primitive, so it costs a full non-inlinable call on every
        ; step of this split.
        (let split ((slow xs) (fast xs) (front '()))
          (if (or (null? fast) (null? (cdr fast)))
              (let ((left (reverse front))
                    (right slow))
                (cond ((null? right) left)
                      ((null? (cdr right)) (%merge-nocheck compare left right))
                      (else (%merge-nocheck compare
                                            (sort-list left)
                                            (sort-list right)))))
              (split (cdr slow) (cdr (cdr fast))
                     (cons (car slow) front))))))))

(define (stable-sort sequence less? . maybe-key)
  (if (> (length maybe-key) 1)
      (error "stable-sort: too many arguments"))
  (if (vector? sequence)
      (list->vector (apply list-sort less? (vector->list sequence)
                           maybe-key))
      (apply list-sort less? sequence maybe-key)))

(define sort stable-sort)

(define (vector-sort less? vec . maybe-key)
  (if (> (length maybe-key) 1)
      (error "vector-sort: too many arguments"))
  (list->vector (apply list-sort less? (vector->list vec) maybe-key)))

(define (vector-sort! less? vec . maybe-key)
  (if (> (length maybe-key) 1)
      (error "vector-sort!: too many arguments"))
  (let ((sorted (apply vector-sort less? vec maybe-key)))
    (let loop ((i 0))
      (if (< i (vector-length vec))
          (begin
            (vector-set! vec i (vector-ref sorted i))
            (loop (+ i 1)))
          vec))))

(define (sort! sequence less? . maybe-key)
  (if (> (length maybe-key) 1)
      (error "sort!: too many arguments"))
  (if (vector? sequence)
      (apply vector-sort! less? sequence maybe-key)
      (error "sort!: expected vector")))

;;; ============================================================================
; MIT-compatible names.  The stable merge-sort implementation is used for
; both public non-destructive algorithms in this compact runtime.
(define (merge-sort sequence less? . maybe-key)
  (apply stable-sort sequence less? maybe-key))
(define (quick-sort sequence less? . maybe-key)
  (apply stable-sort sequence less? maybe-key))
(define merge-sort! sort!)
(define quick-sort! sort!)

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
                        ((char=? directive #\&)
                         (newline port)
                         (loop next args))
                        ((char=? directive #\_)
                         (write-char #\space port)
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

;;; ============================================================================
;;; Character sets (MIT/GNU Scheme character-set operations)
;;; ============================================================================

; Character sets are represented as a tagged vector.  Finite sets built from
; characters, strings, and ranges use a sorted, disjoint range list.  Unicode
; property sets and set combinations use a predicate payload so construction
; does not eagerly allocate a million-code-point table.
(define %char-code-limit #x110000)

(define (%make-char-set kind payload)
  (vector 'char-set kind payload))

(define (%char-set-range-well-formed? element)
  (and (pair? element)
       (not (pair? (cdr element)))
       (exact? (car element))
       (integer? (car element))
       (exact? (cdr element))
       (integer? (cdr element))
       (>= (car element) 0)
       (<= (car element) (cdr element))
       (<= (cdr element) %char-code-limit)))

(define (%char-set-ranges-well-formed? ranges)
  (and (proper-list? ranges)
       (let loop ((ranges ranges))
         (or (null? ranges)
             (and (%char-set-range-well-formed? (car ranges))
                  (loop (cdr ranges)))))))

(define (char-set? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) 'char-set)
       (cond
         ((eq? (vector-ref obj 1) 'ranges)
          (%char-set-ranges-well-formed? (vector-ref obj 2)))
         ((eq? (vector-ref obj 1) 'predicate)
          (procedure? (vector-ref obj 2)))
         (else #f))))

(define (%require-char-set who obj)
  (if (char-set? obj)
      obj
      (error (string-append who ": expected character set"))))

(define (%char-set-range-insert ranges start end)
  (if (= start end)
      ranges
      (if (null? ranges)
          (list (cons start end))
          (let ((range (car ranges)))
            (cond
              ((< end (car range))
               (cons (cons start end) ranges))
              ((> start (cdr range))
               (cons range
                     (%char-set-range-insert (cdr ranges) start end)))
              (else
               (%char-set-range-insert
                (cdr ranges)
                (min start (car range))
                (max end (cdr range)))))))))

(define (%char-set-range-element? code element)
  (and (%char-set-range-well-formed? element)
       (>= code (car element))
       (< code (cdr element))))

(define (%char-set-code-point->char code)
  ; Vesper intentionally rejects surrogate characters in integer->char, so
  ; property predicates treat those code points as absent rather than
  ; attempting to construct an invalid character.
  (if (or (and (>= code #xD800) (< code #xE000))
          (>= code %char-code-limit))
      #f
      (integer->char code)))

(define (%unicode-code-point? code)
  (and (>= code 0)
       (< code %char-code-limit)
       (or (< code #xD800)
           (>= code #xE000))
       (%unicode-assigned? (integer->char code))))

(define (%char-set-named-predicate name)
  (cond
    ((eq? name 'alphabetic)
     (lambda (ch) (and ch (char-alphabetic? ch))))
    ((eq? name 'alphanumeric)
     (lambda (ch) (and ch (or (char-alphabetic? ch) (char-numeric? ch)))))
    ((eq? name 'cased)
     (lambda (ch) (and ch (or (char-upper-case? ch) (char-lower-case? ch)))))
    ((eq? name 'lower-case)
     (lambda (ch) (and ch (char-lower-case? ch))))
    ((eq? name 'numeric)
     (lambda (ch) (and ch (char-numeric? ch))))
    ((eq? name 'upper-case)
     (lambda (ch) (and ch (char-upper-case? ch))))
    ((eq? name 'whitespace)
     (lambda (ch) (and ch (char-whitespace? ch))))
    ((eq? name 'unicode)
     (lambda (ch)
       (and ch (%unicode-code-point? (char->integer ch)))))
    (else #f)))

(define (%char-set-element-member? code element)
  (cond
    ((char? element) (= code (char->integer element)))
    ((and (exact? element) (integer? element)
          (>= element 0) (< element %char-code-limit))
     (= code element))
    ((string? element)
     (let loop ((i 0))
       (and (< i (string-length element))
            (or (= code (char->integer (string-ref element i)))
                (loop (+ i 1))))))
    ((char-set? element) (code-point-in-set? code element))
    ((and (pair? element) (not (pair? (cdr element))))
     (%char-set-range-element? code element))
    ((symbol? element)
     (let ((pred (%char-set-named-predicate element)))
       (and pred (pred (%char-set-code-point->char code)))))
    (else #f)))

(define (%char-set-finite-element-range element)
  (cond
    ((char? element)
     (list (cons (char->integer element) (+ (char->integer element) 1))))
    ((and (exact? element) (integer? element)
          (>= element 0) (< element %char-code-limit))
     (list (cons element (+ element 1))))
    ((and (pair? element) (not (pair? (cdr element))))
     (if (and (exact? (car element)) (integer? (car element))
              (exact? (cdr element)) (integer? (cdr element))
              (>= (car element) 0)
              (<= (car element) (cdr element))
              (<= (cdr element) %char-code-limit))
         (list element)
         (error "char-set: invalid code-point range")))
    ((string? element)
     (let loop ((i 0) (ranges '()))
       (if (= i (string-length element))
           ranges
           (let ((code (char->integer (string-ref element i))))
             (loop (+ i 1)
                   (%char-set-range-insert ranges code (+ code 1)))))))
    ((char-set? element)
     (if (eq? (vector-ref element 1) 'ranges)
         (vector-ref element 2)
         #f))
    (else #f)))

(define (char-set . elements)
  (for-each
    (lambda (element)
      (if (and (symbol? element)
               (not (%char-set-named-predicate element)))
          (error "char-set: unknown character-set name" element)))
    elements)
  (let has-predicate? ((xs elements))
    (if (null? xs)
        (let loop ((xs elements) (ranges '()))
          (if (null? xs)
              (%make-char-set 'ranges ranges)
              (let add ((rs (%char-set-finite-element-range (car xs)))
                        (out ranges))
                (if (null? rs)
                    (loop (cdr xs) out)
                    (add (cdr rs)
                         (%char-set-range-insert
                          out (car (car rs)) (cdr (car rs))))))))
        (if (or (symbol? (car xs))
                (and (char-set? (car xs))
                     (eq? (vector-ref (car xs) 1) 'predicate)))
            (%make-char-set
             'predicate
             (lambda (code)
               (let scan ((ys elements))
                 (and (pair? ys)
                      (or (%char-set-element-member? code (car ys))
                          (scan (cdr ys)))))))
            (has-predicate? (cdr xs))))))

(define (char-set* elements)
  (%require-proper-list "char-set*" elements)
  (apply char-set elements))

(define (code-point-in-set? code set)
  (%require-char-set "code-point-in-set?" set)
  (if (or (not (exact? code)) (not (integer? code))
          (< code 0) (>= code %char-code-limit))
      (error "code-point-in-set?: expected Unicode code point"))
  (if (eq? (vector-ref set 1) 'predicate)
      ((vector-ref set 2) code)
      (let loop ((ranges (vector-ref set 2)))
        (and (pair? ranges)
             (let ((range (car ranges)))
               (if (< code (car range))
                   #f
                   (or (< code (cdr range))
                       (loop (cdr ranges)))))))))

(define (char-in-set? char set)
  (%require-char-set "char-in-set?" set)
  (if (not (char? char))
      (error "char-in-set?: expected character"))
  (code-point-in-set? (char->integer char) set))

(define (char-set-predicate set)
  (%require-char-set "char-set-predicate" set)
  (lambda (char)
    (and (char? char) (char-in-set? char set))))

(define (compute-char-set predicate)
  (if (not (procedure? predicate))
      (error "compute-char-set: expected predicate"))
  (let loop ((code 0) (ranges '()) (start #f))
    (if (= code %char-code-limit)
        (let ((ranges (if start
                          (%char-set-range-insert ranges start code)
                          ranges)))
          (%make-char-set 'ranges ranges))
        (let ((char (%char-set-code-point->char code)))
          (if (not char)
              (if start
                  (loop (+ code 1)
                        (%char-set-range-insert ranges start code)
                        #f)
                  (loop (+ code 1) ranges #f))
              (let ((member? (predicate char)))
                (if member?
                    (if start
                        (loop (+ code 1) ranges start)
                        (loop (+ code 1) ranges code))
                    (if start
                        (loop (+ code 1)
                              (%char-set-range-insert ranges start code)
                              #f)
                        (loop (+ code 1) ranges #f)))))))))

(define (char-set->code-points set)
  (%require-char-set "char-set->code-points" set)
  (if (eq? (vector-ref set 1) 'predicate)
      (char-set->code-points
       (compute-char-set (lambda (char) ((vector-ref set 2)
                                         (char->integer char)))))
      (let loop ((ranges (vector-ref set 2)) (out '()))
        (if (null? ranges)
            (reverse out)
            (let ((range (car ranges)))
              (loop (cdr ranges)
                    (cons (if (= (+ (car range) 1) (cdr range))
                              (car range)
                              range)
                          out)))))))

(define (char-set=? . sets)
  (if (null? sets)
      #t
      (let ((first (car sets)))
        (%require-char-set "char-set=?" first)
        (let loop ((rest (cdr sets)))
          (if (null? rest)
              #t
              (let ((other (car rest)))
                (%require-char-set "char-set=?" other)
                (and (equal? (char-set->code-points first)
                             (char-set->code-points other))
                     (loop (cdr rest)))))))))

(define (char-set-invert set)
  (%require-char-set "char-set-invert" set)
  (%make-char-set 'predicate
                  (lambda (code) (not (code-point-in-set? code set)))))

(define (char-set-union . sets)
  (%require-proper-list "char-set-union" sets)
  (if (null? sets)
      char-set:empty
      (begin
        (for-each (lambda (set) (%require-char-set "char-set-union" set))
                  sets)
        (%make-char-set 'predicate
                        (lambda (code)
                          (any (lambda (set) (code-point-in-set? code set))
                               sets))))))

(define (char-set-intersection . sets)
  (%require-proper-list "char-set-intersection" sets)
  (if (null? sets)
      char-set:full
      (begin
        (for-each (lambda (set) (%require-char-set "char-set-intersection" set))
                  sets)
        (%make-char-set 'predicate
                        (lambda (code)
                          (every (lambda (set) (code-point-in-set? code set))
                                 sets))))))

(define (char-set-difference first . rest)
  (%require-char-set "char-set-difference" first)
  (for-each (lambda (set) (%require-char-set "char-set-difference" set))
            rest)
  (%make-char-set 'predicate
                  (lambda (code)
                    (and (code-point-in-set? code first)
                         (every (lambda (set)
                                 (not (code-point-in-set? code set)))
                                rest)))))

(define (char-set-union* sets)
  (%require-proper-list "char-set-union*" sets)
  (apply char-set-union sets))

(define (char-set-intersection* sets)
  (%require-proper-list "char-set-intersection*" sets)
  (apply char-set-intersection sets))

(define char-set:empty (%make-char-set 'ranges '()))
(define char-set:full
  (%make-char-set 'predicate (lambda (code) #t)))

(define char-set:alphabetic
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (char-alphabetic? char))))))
(define char-set:numeric
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (char-numeric? char))))))
(define char-set:whitespace
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (char-whitespace? char))))))
(define char-set:not-whitespace
  (char-set-difference char-set:full char-set:whitespace))
(define char-set:upper-case
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (char-upper-case? char))))))
(define char-set:lower-case
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (char-lower-case? char))))))
(define char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))

; Additional SRFI-14 standard sets whose definitions are expressible using
; the character predicates and scalar range supported by this runtime.
(define char-set:letter char-set:alphabetic)
(define char-set:digit char-set:numeric)
(define char-set:letter+digit char-set:alphanumeric)
(define char-set:ascii (char-set (cons 0 128)))
(define char-set:blank (char-set #\space #\tab))
(define char-set:hex-digit
  (char-set "0123456789abcdefABCDEF"))

; The remaining SRFI-14 standard sets.  The runtime does not expose the full
; Unicode general-category table, so the sets whose definitions require those
; categories use the portable ASCII/control definitions from SRFI-14 while
; composing with the runtime's Unicode-aware base sets where available.
(define char-set:title-case
  (%make-char-set 'predicate
                  (lambda (code)
                    (if (memq code
                             '(453 456 459 498
                               8072 8073 8074 8075 8076 8077 8078 8079
                               8088 8089 8090 8091 8092 8093 8094 8095
                               8104 8105 8106 8107 8108 8109 8110 8111
                               8124 8140 8188))
                        #t
                        #f))))
(define char-set:punctuation
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (%unicode-punctuation? char))))))
(define char-set:symbol
  (%make-char-set 'predicate
                  (lambda (code)
                    (let ((char (%char-set-code-point->char code)))
                      (and char (%unicode-symbol? char))))))
(define char-set:graphic
  (char-set-union char-set:letter char-set:digit
                  char-set:punctuation char-set:symbol))
(define char-set:printing
  (char-set-union char-set:graphic char-set:whitespace))
(define char-set:iso-control
  (%make-char-set 'predicate
                  (lambda (code)
                    (or (< code 32)
                        (and (>= code 127) (< code 160))))))

(define (%char-set-next-code set code)
  (if (eq? (vector-ref set 1) 'ranges)
      (let loop ((ranges (vector-ref set 2)))
        (if (null? ranges)
            #f
            (let ((range (car ranges)))
              (if (>= code (cdr range))
                  (loop (cdr ranges))
                  (max code (car range))))))
      (let loop ((code code))
        (if (>= code %char-code-limit)
            #f
            (if (code-point-in-set? code set)
                code
                (loop (+ code 1)))))))

(define (char-set-cursor set)
  (%require-char-set "char-set-cursor" set)
  (let ((code (%char-set-next-code set 0)))
    (if code
        (vector 'char-set-cursor set code)
        (vector 'char-set-cursor-end set))))

(define (end-of-char-set? cursor)
  (and (vector? cursor)
       (= (vector-length cursor) 2)
       (eq? (vector-ref cursor 0) 'char-set-cursor-end)))

(define (%require-char-set-cursor who cursor)
  (if (or (not (vector? cursor))
          (not (or (= (vector-length cursor) 2)
                   (= (vector-length cursor) 3)))
          (not (eq? (vector-ref cursor 0)
                    (if (= (vector-length cursor) 2)
                        'char-set-cursor-end
                        'char-set-cursor))))
      (error (string-append who ": expected character-set cursor")))
  cursor)

(define (char-set-ref set cursor)
  (%require-char-set "char-set-ref" set)
  (%require-char-set-cursor "char-set-ref" cursor)
  (if (end-of-char-set? cursor)
      (error "char-set-ref: cursor is at end of character set"))
  (if (not (eq? set (vector-ref cursor 1)))
      (error "char-set-ref: cursor belongs to another character set"))
  (integer->char (vector-ref cursor 2)))

(define (char-set-cursor-next set cursor)
  (%require-char-set "char-set-cursor-next" set)
  (%require-char-set-cursor "char-set-cursor-next" cursor)
  (if (end-of-char-set? cursor)
      (error "char-set-cursor-next: cursor is at end of character set"))
  (if (not (eq? set (vector-ref cursor 1)))
      (error "char-set-cursor-next: cursor belongs to another character set"))
  (let ((code (%char-set-next-code set (+ (vector-ref cursor 2) 1))))
    (if code
        (vector 'char-set-cursor set code)
        (vector 'char-set-cursor-end set))))

(define (char-set-fold kons knil set)
  (%require-char-set "char-set-fold" set)
  (let loop ((cursor (char-set-cursor set)) (result knil))
    (if (end-of-char-set? cursor)
        result
        (begin
          (if (not (procedure? kons))
              (error "char-set-fold: expected procedure"))
          (loop (char-set-cursor-next set cursor)
                (kons (char-set-ref set cursor) result))))))

(define (char-set-hash set . maybe-bound)
  (%require-char-set "char-set-hash" set)
  (if (> (length maybe-bound) 1)
      (error "char-set-hash: too many arguments"))
  (let ((bound (if (null? maybe-bound) #f (car maybe-bound))))
    (if (and bound
             (or (not (exact? bound)) (not (integer? bound)) (<= bound 0)))
        (error "char-set-hash: expected positive exact integer bound"))
    (let ((hash (char-set-fold (lambda (char value)
                                 (modulo (+ (* value 33)
                                            (char->integer char))
                                         2147483647))
                               0
                               set)))
      (if bound (modulo hash bound) hash))))

(define (8-bit-char-set? set)
  (%require-char-set "8-bit-char-set?" set)
  (let loop ((ranges (char-set->code-points set)))
    (if (null? ranges)
        #t
        (let ((range (car ranges)))
          (if (and (integer? range) (>= range 256))
              #f
              (if (and (pair? range) (> (cdr range) 256))
                  #f
                  (loop (cdr ranges))))))))

;;; SRFI-14 convenience and update operations.
(define (->char-set object)
  (cond ((char-set? object) object)
        ((char? object) (char-set object))
        ((string? object) (string->char-set object))
        ((pair? object) (list->char-set object))
        ((null? object) (char-set))
        (else (error "->char-set: unsupported object" object))))

(define (list->char-set chars . maybe-base)
  (%require-proper-list "list->char-set" chars)
  (if (> (length maybe-base) 1)
      (error "list->char-set: too many arguments"))
  (for-each (lambda (char)
              (if (not (char? char))
                  (error "list->char-set: expected character")))
            chars)
  (if (null? maybe-base)
      (apply char-set chars)
      (char-set-union (car maybe-base) (apply char-set chars))))

(define (string->char-set string . maybe-base)
  (if (not (string? string))
      (error "string->char-set: expected string"))
  (if (> (length maybe-base) 1)
      (error "string->char-set: too many arguments"))
  (if (null? maybe-base)
      (apply char-set (string->list string))
      (char-set-union (car maybe-base)
                      (apply char-set (string->list string)))))

(define (char-set->list set)
  (%require-char-set "char-set->list" set)
  (let expand ((ranges (char-set->code-points set)) (out '()))
    (if (null? ranges)
        (reverse out)
        (let ((range (car ranges)))
          (if (integer? range)
              (expand (cdr ranges) (cons (integer->char range) out))
              (let fill ((code (car range)) (out out))
                (if (= code (cdr range))
                    (expand (cdr ranges) out)
                    (fill (+ code 1)
                          (cons (integer->char code) out)))))))))

(define (char-set->string set)
  (list->string (char-set->list set)))

(define (char-set-size set)
  (%require-char-set "char-set-size" set)
  (let count ((ranges (char-set->code-points set)) (n 0))
    (if (null? ranges)
        n
        (let ((range (car ranges)))
          (count (cdr ranges)
                 (+ n (if (integer? range)
                          1
                          (- (cdr range) (car range)))))))))

(define (char-set-count predicate set)
  (%require-char-set "char-set-count" set)
  (let loop ((chars (char-set->list set)) (n 0))
    (if (null? chars)
        n
        (begin
          (if (not (procedure? predicate))
              (error "char-set-count: expected predicate"))
          (loop (cdr chars)
                (if (predicate (car chars)) (+ n 1) n))))))

(define (char-set-every predicate set)
  (%require-char-set "char-set-every" set)
  (let loop ((chars (char-set->list set)))
    (if (null? chars)
        #t
        (begin
          (if (not (procedure? predicate))
              (error "char-set-every: expected predicate"))
          (and (predicate (car chars))
               (loop (cdr chars)))))))

(define (char-set-any predicate set)
  (%require-char-set "char-set-any" set)
  (let loop ((chars (char-set->list set)))
    (if (null? chars)
        #f
        (begin
          (if (not (procedure? predicate))
              (error "char-set-any: expected predicate"))
          (or (predicate (car chars))
              (loop (cdr chars)))))))

(define (char-set-copy set)
  (%require-char-set "char-set-copy" set)
  (%make-char-set (vector-ref set 1) (vector-ref set 2)))

(define (char-set-for-each procedure set)
  (%require-char-set "char-set-for-each" set)
  (for-each procedure (char-set->list set)))

(define (char-set-map procedure set)
  (%require-char-set "char-set-map" set)
  (list->char-set (map procedure (char-set->list set))))

(define (char-set-unfold predicate mapper successor seed . maybe-base)
  (if (or (not (procedure? predicate))
          (not (procedure? mapper))
          (not (procedure? successor)))
      (error "char-set-unfold: expected procedures"))
  (if (> (length maybe-base) 1)
      (error "char-set-unfold: too many arguments"))
  (let ((base (if (null? maybe-base)
                  (char-set)
                  (%require-char-set "char-set-unfold" (car maybe-base)))))
    (let loop ((value seed) (set base))
      (if (predicate value)
          (char-set-copy set)
          (loop (successor value)
                (char-set-adjoin set (mapper value)))))))

(define (char-set-filter predicate set . maybe-base)
  (%require-char-set "char-set-filter" set)
  (if (> (length maybe-base) 1)
      (error "char-set-filter: too many arguments"))
  (let ((base (if (null? maybe-base)
                  (char-set)
                  (%require-char-set "char-set-filter" (car maybe-base)))))
    (char-set-union base
                    (list->char-set
                     (let loop ((chars (char-set->list set)) (out '()))
                       (if (null? chars)
                           (reverse out)
                           (begin
                             ; MIT permits an invalid predicate when the
                             ; filtered set is empty, since it is never
                             ; called.  Validate lazily at the first element.
                             (if (not (procedure? predicate))
                                 (error "char-set-filter: expected predicate"))
                             (loop (cdr chars)
                                   (if (predicate (car chars))
                                       (cons (car chars) out)
                                       out)))))))))

(define (ucs-range->char-set lower upper . args)
  (if (or (not (exact? lower)) (not (integer? lower)) (< lower 0)
          (not (exact? upper)) (not (integer? upper)) (< upper lower))
      (error "ucs-range->char-set: invalid range"))
  (if (> (length args) 2)
      (error "ucs-range->char-set: too many arguments"))
  (let ((error? (if (null? args) #f (car args)))
        (base (if (< (length args) 2)
                  (char-set)
                  (%require-char-set "ucs-range->char-set" (cadr args)))))
    (if (not (boolean? error?))
        (error "ucs-range->char-set: expected boolean"))
    (if (and error?
             (or (>= lower %char-code-limit)
                 (> upper %char-code-limit)
                 (and (< lower #xE000) (> upper #xD800))))
        (error "ucs-range->char-set: invalid Unicode code point"))
    ; Bounds above the Unicode limit are harmless when ERROR? is false, but
    ; must not turn this into a loop over an arbitrary bignum-sized interval.
    ; Scalar code points form at most two intervals here, split by UTF-16's
    ; surrogate block, so construct those ranges directly.
    (let* ((scan-upper (min upper %char-code-limit))
           (first-upper (min scan-upper #xD800))
           (second-lower (max lower #xE000))
           (ranges (if (< lower first-upper)
                       (%char-set-range-insert '() lower first-upper)
                       '()))
           (ranges (if (< second-lower scan-upper)
                       (%char-set-range-insert ranges second-lower scan-upper)
                       ranges)))
      (if (null? (char-set->code-points base))
          (%make-char-set 'ranges ranges)
          (char-set-union base (%make-char-set 'ranges ranges))))))

(define (char-set-adjoin set . chars)
  (%require-char-set "char-set-adjoin" set)
  (for-each (lambda (char)
              (if (not (char? char))
                  (error "char-set-adjoin: expected character")))
            chars)
  (if (eq? (vector-ref set 1) 'ranges)
      (let loop ((chars chars) (ranges (vector-ref set 2)))
        (if (null? chars)
            (%make-char-set 'ranges ranges)
            (let ((code (char->integer (car chars))))
              (loop (cdr chars)
                    (%char-set-range-insert ranges code (+ code 1))))))
      (char-set-union set (apply char-set chars))))

(define (char-set-delete set . chars)
  (%require-char-set "char-set-delete" set)
  (char-set-difference set (apply char-set chars)))

(define (char-set-contains? set char)
  (char-in-set? char set))

(define char-set-complement char-set-invert)

(define (char-set-xor . sets)
  (if (null? sets)
      char-set:empty
      (begin
        (for-each (lambda (set) (%require-char-set "char-set-xor" set)) sets)
        (%make-char-set
         'predicate
         (lambda (code)
           (let parity ((sets sets) (odd? #f))
             (if (null? sets)
                 odd?
                 (parity (cdr sets)
                         (if (code-point-in-set? code (car sets))
                             (not odd?)
                             odd?)))))))))

(define (char-set<= . sets)
  (if (< (length sets) 2)
      #t
      (begin
        (for-each (lambda (set) (%require-char-set "char-set<=" set)) sets)
        (let loop ((sets sets))
          (or (null? (cdr sets))
              (and (let ((left (car sets)) (right (cadr sets)))
                     (char-set-every (lambda (char) (char-in-set? char right))
                                     left))
                   (loop (cdr sets))))))))

(define (char-set= . sets)
  (apply char-set=? sets))

(define (char-set-diff+intersection first second . rest)
  (%require-char-set "char-set-diff+intersection" first)
  (%require-char-set "char-set-diff+intersection" second)
  (for-each (lambda (set) (%require-char-set "char-set-diff+intersection" set))
            rest)
  (values (apply char-set-difference first second rest)
          (apply char-set-intersection first (cons second rest))))

; SRFI-14 linear-update operations may be pure in this representation.
(define char-set-adjoin! char-set-adjoin)
(define char-set-delete! char-set-delete)
(define char-set-complement! char-set-complement)
(define char-set-union! char-set-union)
(define char-set-intersection! char-set-intersection)

; SRFI-14 linear-update constructors are allowed to be pure.  Keep their
; required base-set arguments explicit so arity errors remain visible.
(define (list->char-set! chars base)
  (list->char-set chars base))

(define (string->char-set! string base)
  (string->char-set string base))

(define (char-set-filter! predicate set base)
  (char-set-filter predicate set base))

(define (char-set-unfold! predicate mapper successor seed base)
  (char-set-unfold predicate mapper successor seed base))

(define (ucs-range->char-set! lower upper error? base)
  (ucs-range->char-set lower upper error? base))

(define (char-set-difference! first . rest)
  (apply char-set-difference first rest))

(define (char-set-xor! . sets)
  (apply char-set-xor sets))

(define (char-set-diff+intersection! first second . rest)
  (apply char-set-diff+intersection first second rest))

(define (read-delimited-string set . maybe-port)
  (%require-char-set "read-delimited-string" set)
  (if (> (length maybe-port) 1)
      (error "read-delimited-string: too many arguments"))
  (let ((port (if (null? maybe-port)
                  (current-input-port)
                  (car maybe-port))))
    (let loop ((chars '()))
      (let ((char (peek-char port)))
        (cond
          ((eof-object? char)
           (if (null? chars) char (list->string (reverse chars))))
          ((char-in-set? char set)
           (list->string (reverse chars)))
          (else
           (read-char port)
           (loop (cons char chars))))))))

; MIT/GNU Scheme historically accepted (read-string char-set [port]) in
; addition to the standard count-based form.  Preserve both interfaces while
; keeping the primitive implementation for the count-based operation.
(define %read-string-primitive read-string)
(define (read-string first . maybe-port)
  (if (char-set? first)
      (apply read-delimited-string first maybe-port)
      (apply %read-string-primitive first maybe-port)))

(define (discard-chars set . maybe-port)
  (%require-char-set "discard-chars" set)
  (if (> (length maybe-port) 1)
      (error "discard-chars: too many arguments"))
  (let ((port (if (null? maybe-port)
                  (current-input-port)
                  (car maybe-port))))
    (let loop ()
      (let ((char (peek-char port)))
        (cond ((eof-object? char) #f)
              ((char-in-set? char set) #f)
              (else (read-char port) (loop)))))))

(define (read-string! string . args)
  (if (not (string? string))
      (error "read-string!: expected string"))
  (if (> (length args) 3)
      (error "read-string!: too many arguments"))
  (let* ((port (if (null? args) (current-input-port) (car args)))
         (start (if (< (length args) 2) 0 (cadr args)))
         (end (if (< (length args) 3) (string-length string) (caddr args))))
    ; Validate the port even for an empty range.  Otherwise malformed or
    ; binary ports are silently accepted when the loop has no characters to
    ; read.
    (if (not (input-port? port))
        (error "read-string!: expected input port"))
    (if (not (textual-port? port))
        (error "read-string!: expected textual input port"))
    (if (or (not (exact? start)) (not (integer? start)) (< start 0)
            (not (exact? end)) (not (integer? end)) (< end start)
            (> end (string-length string)))
        (error "read-string!: invalid string range"))
    (let loop ((index start))
      (if (= index end)
          (- index start)
          (let ((char (read-char port)))
            (if (eof-object? char)
                (if (= index start)
                    char
                    (- index start))
                (begin
                  (string-set! string index char)
                  (loop (+ index 1)))))))))

(define (read-substring! string start end . maybe-port)
  (if (> (length maybe-port) 1)
      (error "read-substring!: too many arguments"))
  (if (null? maybe-port)
      (read-string! string (current-input-port) start end)
      (read-string! string (car maybe-port) start end)))

(define (string-normalized-nfd? s)
  (string=? s (string-normalize-nfd s)))

(define (string-normalized-nfc? s)
  (string=? s (string-normalize-nfc s)))

(define (string-normalized-nfkd? s)
  (string=? s (string-normalize-nfkd s)))

(define (string-normalized-nfkc? s)
  (string=? s (string-normalize-nfkc s)))

;; MIT Scheme normalization names.
(define string->nfd string-normalize-nfd)
(define string->nfc string-normalize-nfc)
(define string->nfkd string-normalize-nfkd)
(define string->nfkc string-normalize-nfkc)
(define string-in-nfd? string-normalized-nfd?)
(define string-in-nfc? string-normalized-nfc?)

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

(define (string-prefix-ci? prefix s)
  (string-prefix? (string-foldcase prefix) (string-foldcase s)))

(define (string-suffix-ci? suffix s)
  (string-suffix? (string-foldcase suffix) (string-foldcase s)))

;; string-trim and friends retain characters in the optional character set.
;; The default retains every non-whitespace character, matching MIT Scheme.
(define (string-trim s . maybe-set)
  (if (> (length maybe-set) 1)
      (error "string-trim: expected at most one character set"))
  (let ((set (if (null? maybe-set) char-set:not-whitespace (car maybe-set))))
    (%require-char-set "string-trim" set)
    ((string-trimmer 'to-trim
                     (lambda (ch) (not (char-in-set? ch set)))) s)))

;; string-trim-left - remove leading whitespace
(define (string-trim-left s . maybe-set)
  (if (> (length maybe-set) 1)
      (error "string-trim-left: expected at most one character set"))
  (let ((set (if (null? maybe-set) char-set:not-whitespace (car maybe-set))))
    (%require-char-set "string-trim-left" set)
    ((string-trimmer 'where 'leading
                     'to-trim
                     (lambda (ch) (not (char-in-set? ch set)))) s)))

;; string-trim-right - remove trailing whitespace
(define (string-trim-right s . maybe-set)
  (if (> (length maybe-set) 1)
      (error "string-trim-right: expected at most one character set"))
  (let ((set (if (null? maybe-set) char-set:not-whitespace (car maybe-set))))
    (%require-char-set "string-trim-right" set)
    ((string-trimmer 'where 'trailing
                     'to-trim
                     (lambda (ch) (not (char-in-set? ch set)))) s)))

;; string-split - split string by delimiter (default: whitespace)
(define (string-split s . args)
  (if (> (length args) 1)
      (error "string-split: expected at most one delimiter"))
  (let ((delim (if (null? args) #f (car args))))
    ; The omitted delimiter selects whitespace splitting.  An explicitly
    ; supplied delimiter must still be a character, even when the input is
    ; empty (otherwise invalid arguments are silently accepted).
    (if (and (pair? args) (not (char? delim)))
        (error "string-split: delimiter must be a character"))
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
    ((%define-values-declare! ()) '())
    ((%define-values-declare! (var . more))
     (begin (define var #f) (%define-values-declare! more)))
    ((%define-values-declare! var) (define var #f))))

(define-syntax define-values-set!
  (syntax-rules ()
    ((define-values-set! () vals)
     (if (null? vals)
         '()
         (error "define-values: too many values")))
    ((define-values-set! (var . more) vals)
     (if (pair? vals)
         (begin
           (set! var (car vals))
           (define-values-set! more (cdr vals)))
         (error "define-values: too few values")))
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

;; Evaluate every producer before introducing any of the bound variables.
;; A direct ellipsis expansion using one identifier such as `vals` creates
;; duplicate lambda formals for two or more clauses.  Store all producers as
;; closures first; each closure captures the outer lexical environment, so
;; evaluating them later still has parallel (not let*-values) visibility.
(define-syntax let-values-bind-from-producers
  (syntax-rules ()
    ((let-values-bind-from-producers () producers body ...)
     (begin body ...))
    ((let-values-bind-from-producers ((formals ignored) rest ...)
                                      producers body ...)
     (call-with-values
       (car producers)
       (lambda formals
         (let-values-bind-from-producers (rest ...) (cdr producers) body ...))))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values ((formals expression) ...) body ...)
     (let ((producers (list (lambda () expression) ...)))
       (let-values-bind-from-producers ((formals expression) ...)
                                        producers body ...)))))

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

(define %hash-table-ref-primitive hash-table-ref)

(define (hash-table-ref table key . maybe-get-default)
  (if (> (length maybe-get-default) 1)
      (error "hash-table-ref: too many arguments"))
  (if (hash-table-exists? table key)
      (%hash-table-ref-primitive table key)
      (if (null? maybe-get-default)
          (%hash-table-ref-primitive table key)
          ((car maybe-get-default)))))

(define (alist->hash-table alist . args)
  (if (not (null? args))
      (error "alist->hash-table: unsupported optional arguments"))
  (%require-proper-list "alist->hash-table" alist)
  (let ((table (make-hash-table)))
    (for-each
     (lambda (entry)
       (if (not (pair? entry))
           (error "alist->hash-table: expected an association list"))
       (hash-table-set! table (car entry) (cdr entry)))
     alist)
    table))

(define (hash-table-ref/default table key default)
  (if (hash-table-exists? table key)
      (%hash-table-ref-primitive table key)
      default))

(define (hash-table-hash-function table)
  (lambda (key . maybe-bound)
    (if (> (length maybe-bound) 1)
        (error "hash-table-hash-function: expected at most one bound"))
    (if (null? maybe-bound)
        (%hash-table-hash table key)
        (%hash-table-hash table key (car maybe-bound)))))

(define (hash-table-update!/default table key proc default)
  (hash-table-update! table key proc (lambda () default)))

(define (hash-table-update! table key proc . maybe-get-default)
  (if (> (length maybe-get-default) 1)
      (error "hash-table-update!: too many arguments"))
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

(define (hash-table-merge! table1 table2)
  (hash-table-walk table2
                   (lambda (key value)
                     (hash-table-set! table1 key value)))
  table1)

(define (hash-table-for-each proc table)
  (hash-table-walk table proc))

(define (hash-table-fold table proc init)
  (let loop ((entries (hash-table->alist table)) (acc init))
    (if (null? entries)
        acc
        (let ((entry (car entries)))
          (loop (cdr entries)
                (proc (car entry) (cdr entry) acc))))))

(define (hash-table-map proc table)
  (if (not (procedure? proc))
      (error "hash-table-map: expected procedure"))
  (map (lambda (entry) (proc (car entry) (cdr entry)))
       (hash-table->alist table)))

;;; ============================================================================
;;; Vector and string library additions
;;; ============================================================================

(define (%require-vector-range who vec start end)
  (if (not (vector? vec))
      (error (string-append who ": expected vector")))
  (%require-nonnegative-integer who start)
  (%require-nonnegative-integer who end)
  (let ((len (vector-length vec)))
    (if (or (> start end) (> end len))
        (error (string-append who ": invalid vector range"))))
  #t)

(define (vector-copy vec . rest)
  (if (> (length rest) 2)
      (error "vector-copy: too many arguments"))
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (if (vector? vec)
                                                 (vector-length vec)
                                                 0))))
    (%require-vector-range "vector-copy" vec start end)
    (let* ((len (- end start))
           (out (make-vector len)))
    (let loop ((i 0))
      (if (= i len)
          out
          (begin
            (vector-set! out i (vector-ref vec (+ start i)))
            (loop (+ i 1))))))))

(define (vector-copy! to at from . rest)
  (if (> (length rest) 2)
      (error "vector-copy!: too many arguments"))
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2) (if (vector? from)
                                                 (vector-length from)
                                                 0))))
    (%require-vector-range "vector-copy!" from start end)
    (if (not (vector? to))
        (error "vector-copy!: expected destination vector"))
    (%require-nonnegative-integer "vector-copy!" at)
    (let* ((len (- end start))
           (to-len (vector-length to)))
      (if (> at to-len)
          (error "vector-copy!: destination index out of bounds"))
      (if (> len (- to-len at))
          (error "vector-copy!: destination too short"))
      ; Copy through a temporary vector so overlapping source and destination
      ; ranges have the same well-defined behavior as the documented API.
      (let ((tmp (vector-copy from start end)))
      (let loop ((i 0))
        (if (= i len)
           '()
          (begin
            (vector-set! to (+ at i) (vector-ref tmp i))
            (loop (+ i 1)))))))))

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
  (%require-proper-list "string*" objects)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (obj) (display obj port)) objects))))

(define (string-append* strings)
  (%require-proper-list "string-append*" strings)
  (for-each
    (lambda (string)
      (if (not (string? string))
          (error "string-append*: expected a list of strings")))
    strings)
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
  (let loop ((chars (string->list str)))
    (cond ((null? chars) #t)
          ((char-alphabetic? (car chars))
           (and (char-upper-case? (car chars))
                (loop (cdr chars))))
          (else
           (loop (cdr chars))))))

(define (string-lower-case? str)
  (let loop ((chars (string->list str)))
    (cond ((null? chars) #t)
          ((char-alphabetic? (car chars))
           (and (char-lower-case? (car chars))
                (loop (cdr chars))))
          (else
           (loop (cdr chars))))))

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
  (if (> (length maybe-modulus) 1)
      (error "string-hash: expected at most one modulus"))
  (let ((has-modulus? (pair? maybe-modulus))
        (modulus (if (pair? maybe-modulus) (car maybe-modulus) #f)))
    ;; A supplied #f is still an argument and must not be confused with
    ;; the omitted-modulus case.
    (if (and has-modulus?
             (or (not (exact-integer? modulus))
                 (<= modulus 0)))
        (error "string-hash: modulus must be a positive integer"))
    ; MIT/GNU Scheme's SRFI-69 hash is FNV-1a over each character's
    ; minimal little-endian byte representation, reduced to 32 bits after
    ; every byte.  Processing code points rather than UTF-8 bytes is
    ; important for compatibility with non-ASCII strings.
    (let loop ((chars (string->list str)) (hash 2166136261))
      (if (null? chars)
          (if has-modulus? (modulo hash modulus) hash)
          (let byte-loop ((code (char->integer (car chars)))
                          (hash hash))
            (let ((next (modulo
                         (* (bitwise-xor hash (modulo code 256))
                            16777619)
                         4294967296)))
              (if (< code 256)
                  (loop (cdr chars) next)
                  (byte-loop (quotient code 256) next))))))))

(define (string-hash-ci str . maybe-modulus)
  (apply string-hash (string-foldcase str) maybe-modulus))

;; SRFI-69 names the case-insensitive variant string-ci-hash.  Keep the
;; existing MIT-compatible spelling as an alias as well.
(define string-ci-hash string-hash-ci)

(define (string-builder . maybe-buffer-length)
  (if (or (> (length maybe-buffer-length) 1)
          (and (pair? maybe-buffer-length)
               (or (not (exact-nonnegative-integer?
                          (car maybe-buffer-length)))
                   (= (car maybe-buffer-length) 0))))
      (error "string-builder: buffer length must be a positive integer"))
  (let ((chunks '())
        (count 0))
    ; Keep pieces as a list while building, but finalize through a string
    ; port.  Applying string-append to every piece imposes the VM's argument
    ; limit and needlessly constructs a large argument frame.
    (let ((finish
           (lambda ()
             (call-with-output-string
               (lambda (port)
                 (for-each (lambda (piece) (display piece port))
                           (reverse chunks)))))))
      (lambda args
      (if (> (length args) 1)
          (error "string-builder: expected at most one argument"))
      (cond
        ((null? args)
         (finish))
        ((eq? (car args) 'immutable)
         (finish))
        ((eq? (car args) 'mutable)
         (string-copy (finish)))
        ((eq? (car args) 'nfc)
         (string-normalize-nfc (finish)))
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
                (car args))))))))

(define (check-keyword-args args keys name)
  (let loop ((xs args) (seen '()))
    (if (null? xs)
        #t
        (if (null? (cdr xs))
            (error name "keyword argument missing value")
            (let ((key (car xs)))
              (if (or (not (symbol? key))
                      (not (memq key keys)))
                  (error name "unknown keyword" key)
                  (if (memq key seen)
                      (error name "duplicate keyword" key)
                      (loop (cddr xs) (cons key seen)))))))))

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
  (%require-proper-list "string-join" strings)
  (for-each
    (lambda (string)
      (if (not (string? string))
          (error "string-join: expected a list of strings")))
    strings)
  (if (> (length args) 3)
      (error "string-join: expected at most separator, prefix, and suffix"))
  (let* ((infix (if (pair? args) (car args) " "))
         (rest1 (if (pair? args) (cdr args) '()))
         (prefix (if (pair? rest1) (car rest1) ""))
         (rest2 (if (pair? rest1) (cdr rest1) '()))
         (suffix (if (pair? rest2) (car rest2) "")))
    (if (or (not (string? infix))
            (not (string? prefix))
            (not (string? suffix)))
        (error "string-join: expected string separator, prefix, and suffix"))
    ; Write each component once.  Repeated string-append would copy the
    ; growing prefix on every iteration, making long joins quadratic.
    (call-with-output-string
     (lambda (port)
       (write-string prefix port)
       (let loop ((xs strings) (first? #t))
         (if (null? xs)
             (write-string suffix port)
             (begin
               (if (not first?)
                   (write-string infix port))
               (write-string (car xs) port)
               (loop (cdr xs) #f))))))))

(define (string-joiner . args)
  (check-keyword-args args '(infix prefix suffix) "string-joiner")
  (let ((infix (keyword-arg args 'infix ""))
        (prefix (keyword-arg args 'prefix ""))
        (suffix (keyword-arg args 'suffix "")))
    (if (or (not (string? infix))
            (not (string? prefix))
            (not (string? suffix)))
        (error "string-joiner: infix, prefix, and suffix must be strings"))
    (lambda strings
      (string-join strings infix prefix suffix))))

(define (string-joiner* . args)
  (let ((joiner (apply string-joiner args)))
    (lambda (strings)
      (apply joiner strings))))

(define (delimiter-procedure delimiter)
  (cond ((procedure? delimiter) delimiter)
        ((char? delimiter) (lambda (ch) (char=? ch delimiter)))
        ((char-set? delimiter)
         (lambda (ch) (char-in-set? ch delimiter)))
        (else (error "string-splitter: unsupported delimiter" delimiter))))

(define (string-splitter . args)
  (check-keyword-args args '(delimiter allow-runs? copier copy?) "string-splitter")
  (let ((delimiter (delimiter-procedure
                     (keyword-arg args 'delimiter char-whitespace?)))
        (allow-runs? (keyword-arg args 'allow-runs? #t))
        (copy? (keyword-arg args 'copy? #f))
        (copier (keyword-arg args 'copier string-slice)))
    (if (not (boolean? allow-runs?))
        (error "string-splitter: allow-runs? must be boolean"))
    (if (not (boolean? copy?))
        (error "string-splitter: copy? must be boolean"))
    (if (not (procedure? copier))
        (error "string-splitter: copier must be a procedure"))
    ; MIT validates an explicitly supplied copier even when copy? would
    ; otherwise replace it with substring.
    (if copy? (set! copier substring))
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

;; A grapheme here follows the common extended-cluster cases needed by the
;; padding utilities: combining/variation marks, emoji modifiers, ZWJ joins,
;; and paired regional indicators.  The internal predicate is backed by
;; Unicode combining classes.  Ordinary string APIs remain code-point based.
(define (%code-point-between? code low high)
  (and (<= low code) (<= code high)))

(define (%string-grapheme-extend? char)
  (let ((code (char->integer char)))
    (or (%char-combining? char)
        (%code-point-between? code #xFE00 #xFE0F)
        (%code-point-between? code #xE0100 #xE01EF)
        (%code-point-between? code #x1F3FB #x1F3FF))))

(define (%string-grapheme-regional-indicator? char)
  (%code-point-between? (char->integer char) #x1F1E6 #x1F1FF))

(define (%string-grapheme-boundaries str)
  (let ((length (string-length str)))
    (let loop ((i 0)
               (boundaries (if (> length 0) '(0) '()))
               (previous #f)
               (regional-run 0))
      (if (= i length)
          (reverse (cons length boundaries))
          (let* ((current (string-ref str i))
                 (regional? (%string-grapheme-regional-indicator? current))
                 (previous-regional?
                  (and previous
                       (%string-grapheme-regional-indicator? previous)))
                 (joined? (and previous (char=? previous #\x200d)))
                 (current-zwj? (char=? current #\x200d))
                 (paired-regional?
                  (and regional? previous-regional? (odd? regional-run)))
                 (boundary?
                  (and (> i 0)
                       (not (or (%string-grapheme-extend? current)
                                joined?
                                current-zwj?
                                paired-regional?))))
                 (next-boundaries (if boundary?
                                     (cons i boundaries)
                                     boundaries))
                 (next-regional-run (if regional? (+ regional-run 1) 0)))
            (loop (+ i 1) next-boundaries current next-regional-run))))))

(define (%string-grapheme-count str)
  (- (length (%string-grapheme-boundaries str)) 1))

(define (string-padder . args)
  (check-keyword-args args '(where fill-with clip?) "string-padder")
  (let ((where (keyword-arg args 'where 'leading))
        (fill-with (keyword-arg args 'fill-with " "))
        (clip? (keyword-arg args 'clip? #t)))
    (if (not (or (eq? where 'leading) (eq? where 'trailing)))
        (error "string-padder: where must be leading or trailing"))
    (if (or (not (string? fill-with))
            (not (= (%string-grapheme-count fill-with) 1)))
        (error "string-padder: fill-with must contain exactly one grapheme"))
    (if (not (boolean? clip?))
        (error "string-padder: clip? must be boolean"))
    (lambda (str len)
      (%require-nonnegative-integer "string-padder" len)
      (let* ((boundaries (%string-grapheme-boundaries str))
             (n (- (length boundaries) 1)))
        (cond
          ((< n len)
           (let ((padding (repeat-string fill-with (- len n))))
             (if (eq? where 'trailing)
                 (string-append str padding)
                 (string-append padding str))))
          ((and clip? (> n len))
           (let ((start (if (eq? where 'trailing)
                            0
                            (list-ref boundaries (- n len))))
                 (end (if (eq? where 'trailing)
                          (list-ref boundaries len)
                          (string-length str))))
             (substring str start end)))
          (else str))))))

(define (string-pad-left str k . maybe-char)
  (if (> (length maybe-char) 1)
      (error "string-pad-left: too many arguments"))
  ((string-padder 'where 'leading
                  'fill-with (string (if (null? maybe-char)
                                         #\space
                                         (car maybe-char))))
   str
   k))

(define (string-pad-right str k . maybe-char)
  (if (> (length maybe-char) 1)
      (error "string-pad-right: too many arguments"))
  ((string-padder 'where 'trailing
                  'fill-with (string (if (null? maybe-char)
                                         #\space
                                         (car maybe-char))))
   str
   k))

(define (trim-procedure to-trim)
  (cond ((procedure? to-trim) to-trim)
        ((char? to-trim) (lambda (ch) (char=? ch to-trim)))
        ((char-set? to-trim) (lambda (ch) (char-in-set? ch to-trim)))
        (else (error "string-trimmer: unsupported trim predicate" to-trim))))

(define (string-trimmer . args)
  (check-keyword-args args '(where to-trim copier copy?) "string-trimmer")
  (let ((where (keyword-arg args 'where 'both))
        (to-trim (trim-procedure (keyword-arg args 'to-trim char-whitespace?)))
        (copy? (keyword-arg args 'copy? #f))
        (copier (keyword-arg args 'copier string-slice)))
    (if (not (or (eq? where 'leading)
                 (eq? where 'trailing)
                 (eq? where 'both)))
        (error "string-trimmer: where must be leading, trailing, or both"))
    (if (not (boolean? copy?))
        (error "string-trimmer: copy? must be boolean"))
    (if (not (procedure? copier))
        (error "string-trimmer: copier must be a procedure"))
    (if copy? (set! copier substring))
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
  (if (not (char? char1))
      (error "string-replace: expected first character"))
  (if (not (char? char2))
      (error "string-replace: expected replacement character"))
  (list->string
    (map (lambda (ch) (if (char=? ch char1) char2 ch))
         (string->list str))))

(define (%require-string-range who str start end)
  (if (not (string? str))
      (error (string-append who ": expected string")))
  (%require-nonnegative-integer who start)
  (%require-nonnegative-integer who end)
  (let ((len (string-length str)))
    (if (or (> start end) (> end len))
        (error (string-append who ": invalid string range"))))
  #t)

(define (string-copy! to at from . rest)
  (if (> (length rest) 2)
      (error "string-copy!: too many arguments"))
  (let* ((start (if (pair? rest) (car rest) 0))
         (rest2 (if (pair? rest) (cdr rest) '()))
         (end (if (pair? rest2) (car rest2)
                  (if (string? from) (string-length from) 0))))
    (%require-string-range "string-copy!" from start end)
    (if (not (string? to))
        (error "string-copy!: expected destination string"))
    ; Force the destination mutability check even when count is zero.  The
    ; empty fill is a no-op for mutable strings and avoids silently accepting
    ; an immutable destination when the copy loop has no iterations.
    (string-fill! to #\space 0 0)
    (%require-nonnegative-integer "string-copy!" at)
    (let ((count (- end start)))
      (if (> at (string-length to))
          (error "string-copy!: destination index out of bounds"))
      (if (> count (- (string-length to) at))
          (error "string-copy!: destination too short"))
      ; Read the source before writing so overlapping source/destination
      ; ranges behave as if the source were copied to a temporary string.
      (let ((temp (string-copy from start end)))
    (let loop ((i 0) (j at))
      (if (= i (string-length temp))
          j
          (begin
            (string-set! to j (string-ref temp i))
            (loop (+ i 1) (+ j 1)))))))))

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
  (let ((returned? #f))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (call-with-values
          (lambda () (proc port))
          (lambda results
            (set! returned? #t)
            (apply values results))))
      (lambda ()
        (if returned?
            (close-port port))))))

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
  (if (not (exact-integer? b))
      (error "modexp: base must be an exact integer"))
  (if (not (exact-nonnegative-integer? e))
      (error "modexp: exponent must be a nonnegative exact integer"))
  (if (not (and (exact-integer? m) (> m 0)))
      (error "modexp: modulus must be a positive exact integer"))
  ; MIT returns 1 for exponent zero even with modulus one.  Reduce only
  ; after an exponent bit is consumed so that identity is preserved.
  (let loop ((base (modulo b m))
             (exp e)
             (result 1))
    (cond
      ((= exp 0) result)
      ((odd? exp)
       (loop (modulo (* base base) m)
             (quotient exp 2)
             (modulo (* result base) m)))
      (else
       (loop (modulo (* base base) m)
             (quotient exp 2)
             result)))))

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
  (%require-proper-list "logsumexp" xs)
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
      ;; Keep newer Unicode decimal ranges in sync with char-numeric?.
      ((and (>= n #x07C0) (<= n #x07C9)) (- n #x07C0))
      ((and (>= n #x1946) (<= n #x194F)) (- n #x1946))
      ((and (>= n #x19D0) (<= n #x19D9)) (- n #x19D0))
      ((and (>= n #x1A80) (<= n #x1A89)) (- n #x1A80))
      ((and (>= n #x1A90) (<= n #x1A99)) (- n #x1A90))
      ((and (>= n #x1BB0) (<= n #x1BB9)) (- n #x1BB0))
      ((and (>= n #x1C40) (<= n #x1C49)) (- n #x1C40))
      ((and (>= n #x1C50) (<= n #x1C59)) (- n #x1C50))
      ((and (>= n #x1D7CE) (<= n #x1D7FF)) (- n #x1D7CE))
      ((and (>= n #x1E140) (<= n #x1E149)) (- n #x1E140))
      ((and (>= n #x1E2F0) (<= n #x1E2F9)) (- n #x1E2F0))
      ((and (>= n #x1E4F0) (<= n #x1E4F9)) (- n #x1E4F0))
      ((and (>= n #x1E950) (<= n #x1E959)) (- n #x1E950))
      ((and (>= n #x1FBF0) (<= n #x1FBF9)) (- n #x1FBF0))
      (else #f))))

(define (char-alphanumeric? ch)
  (if (not (char? ch))
      (error "char-alphanumeric?: expected character"))
  (or (char-alphabetic? ch) (char-numeric? ch)))

(define (char->digit ch . maybe-radix)
  (if (not (char? ch))
      (error "char->digit: expected character"))
  (if (> (length maybe-radix) 1)
      (error "char->digit: too many arguments"))
  (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
    (if (or (not (exact? radix)) (not (integer? radix))
            (< radix 2) (> radix 36))
        (error "char->digit: radix must be an exact integer in [2,36]"))
    (let ((unicode-digit (digit-value ch))
          (code (char->integer ch)))
      (if (not (eq? unicode-digit #f))
          (if (< unicode-digit radix) unicode-digit #f)
          (cond
            ((and (>= code (char->integer #\A))
                  (< code (+ (char->integer #\A) 26)))
             (let ((digit (+ 10 (- code (char->integer #\A)))))
               (if (< digit radix) digit #f)))
            ((and (>= code (char->integer #\a))
                  (< code (+ (char->integer #\a) 26)))
             (let ((digit (+ 10 (- code (char->integer #\a)))))
               (if (< digit radix) digit #f)))
            (else #f))))))

(define (digit->char digit . maybe-radix)
  (if (> (length maybe-radix) 1)
      (error "digit->char: too many arguments"))
  (let ((radix (if (null? maybe-radix) 10 (car maybe-radix))))
    (if (or (not (exact? radix)) (not (integer? radix))
            (< radix 2) (> radix 36))
        (error "digit->char: radix must be an exact integer in [2,36]"))
    (if (or (not (exact? digit)) (not (integer? digit))
            (< digit 0) (>= digit radix))
        (error "digit->char: digit out of range"))
    (integer->char
     (if (< digit 10)
         (+ (char->integer #\0) digit)
         (+ (char->integer #\A) (- digit 10))))))

(define (char->name ch)
  (if (not (char? ch))
      (error "char->name: expected character"))
  (let ((code (char->integer ch)))
    (cond ((= code 7) "alarm")
          ((= code 8) "backspace")
          ((= code 127) "delete")
          ((= code 27) "escape")
          ((= code 10) "newline")
          ((= code 0) "null")
          ((= code 13) "return")
          ((= code 32) "space")
          ((= code 9) "tab")
          ((and (> code 32) (< code 127)) (string ch))
          (else (string-append "x" (number->string code 16))))))

(define (name->char name)
  (if (not (string? name))
      (error "name->char: expected string"))
  (if (= (string-length name) 1)
      (string-ref name 0)
      (let ((special
             (cond ((string-ci=? name "alarm") 7)
                   ((string-ci=? name "backspace") 8)
                   ((string-ci=? name "delete") 127)
                   ((string-ci=? name "escape") 27)
                   ((string-ci=? name "newline") 10)
                   ((string-ci=? name "null") 0)
                   ((string-ci=? name "return") 13)
                   ((string-ci=? name "space") 32)
                   ((string-ci=? name "tab") 9)
                   (else #f))))
        ;; Code point zero is a valid result for "null" but is false in
        ;; Scheme, so distinguish the absent-name sentinel explicitly.
        (if (not (eq? special #f))
            (integer->char special)
            (if (and (> (string-length name) 1)
                     (or (char=? (string-ref name 0) #\x)
                         (char=? (string-ref name 0) #\X)))
                (let ((code (string->number
                             (substring name 1 (string-length name)) 16)))
                  (if (and code (exact? code) (integer? code))
                      (integer->char code)
                      (error "name->char: invalid character name")))
                (error "name->char: invalid character name"))))))

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
            ((char=? (string-ref p i) #\/)
             (if (= i 0) "/" (substring p 0 i)))
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

(define (%valid-r7rs-import-identifiers? identifiers)
  (and (pair? identifiers)
       (proper-list? identifiers)
       (every symbol? identifiers)))

(define (%valid-r7rs-rename-list? renames)
  (and (pair? renames)
       (proper-list? renames)
       (every (lambda (rename)
                (and (proper-list? rename)
                     (= (length rename) 2)
                     (symbol? (car rename))
                     (symbol? (cadr rename))))
              renames)))

(define (supported-r7rs-environment? spec)
  (cond
    ((member spec *supported-r7rs-environments*) #t)
    ((not (pair? spec)) #f)
    ((eq? (car spec) 'only)
     (and (pair? (cdr spec))
          (%valid-r7rs-import-identifiers? (cddr spec))
          (supported-r7rs-environment? (cadr spec))))
    ((eq? (car spec) 'except)
     (and (pair? (cdr spec))
          (%valid-r7rs-import-identifiers? (cddr spec))
          (supported-r7rs-environment? (cadr spec))))
    ((eq? (car spec) 'prefix)
     (and (pair? (cdr spec))
          (pair? (cddr spec))
          (null? (cdddr spec))
          (symbol? (caddr spec))
          (supported-r7rs-environment? (cadr spec))))
    ((eq? (car spec) 'rename)
     (and (pair? (cdr spec))
          (%valid-r7rs-rename-list? (cddr spec))
          (supported-r7rs-environment? (cadr spec))))
    (else #f)))

(define (environment . specs)
  ;; R7RS constructs the result from an empty environment before importing
  ;; the requested libraries.  With no import sets, do not leak the mutable
  ;; interaction environment into eval.
  (if (null? specs)
      (null-environment 5)
      (begin
        (for-each
          (lambda (spec)
            (if (not (supported-r7rs-environment? spec))
                (error "environment: unsupported library specifier" spec)))
          specs)
        (%r7rs-environment specs))))

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

(define (i/o-port? obj)
  (and (input-port? obj) (output-port? obj)))

(define (square x)
  (* x x))

(define (string->vector str . rest)
  (if (> (length rest) 2)
      (error "string->vector: too many arguments"))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (string-length str)
                  (cadr rest))))
    (%require-string-range "string->vector" str start end)
    (let ((vec (make-vector (- end start))))
    (let loop ((i start) (j 0))
      (if (< i end)
          (begin
            (vector-set! vec j (string-ref str i))
            (loop (+ i 1) (+ j 1)))
          vec)))))

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
  (if (> (length rest) 2)
      (error "vector->string: too many arguments"))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (vector-length vec)
                  (cadr rest))))
    (%require-vector-range "vector->string" vec start end)
    (let ((str (make-string (- end start))))
    (let loop ((i start) (j 0))
      (if (< i end)
          (begin
            (string-set! str j (vector-ref vec i))
            (loop (+ i 1) (+ j 1)))
          str)))))

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
