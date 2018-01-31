#lang racket


(require 2htdp/batch-io)
(require test-engine/racket-tests)
(require racket/dict)

(define TEST-FILE "..//Data//wksst8110.fwf")

(define SETTINGS (hash 'check-lines 100
                       'delimiters '(" " "\t"))) ;; settings hash for tests

;; TODO
;; change mask to vector of booleans
;; better than exploding strings L88


;; Settings:
;; The following are the symbols uses for settings hash table
;; 'skiplines, 'delimiters, 'check-lines
;; - checkLines -> positive integer: How many lines to infer columnspecs from. Default 100.
;; - delimeters -> list of strings: Delimiters to skip
;; - skiplines -> natural number: How many lines to skip from the top


;; read-fwf: reads and parses a fixed width file (fwf)
;; src        -> String: Source File Location
;; colspecs   -> list of numbers or "infer": The starting index of the columns. Infer will calculate this on its own.
;; checkLines -> positive integer: How many lines to infer columnspecs from. Default 100.
;; delimeters -> list of strings: Delimiters to skip
;; skiplines  -> natural number: How many lines to skip from the top

(define (read-fwf src [colspecs "infer"] [skiplines 0] [delimiters '(" " "\t")] [check-lines 100])
  (define settings (createSettings skiplines delimiters check-lines))
  (define lines (remove-first-n (read-lines src) skiplines))          ;; reads in lines and removes the first n lines as determined by skiplines
  (if (string=? colspecs "infer")
      (set! colspecs (determine-colspecs lines settings))
      (void))
  (parse-fwf lines colspecs settings))

;; create-settings: creates the settings dictionary for read-fwf
;; see read-fwf for information on parameters

(define (createSettings skiplines delimiters checkLines)
  (hash 'skiplines skiplines
        'delimiters delimiters
        'check-lines checkLines))


;; determine-colspecs: determines the starting indices of the columns
;; lines    -> the lines of the fwf
;; settings -> the hashtable of settings

(define (determine-colspecs lines settings)
  (define max-length (longest-string lines))
  (define mask (initialize-vector (make-vector (+ max-length 1)))) ;; Vector's initialize with zeros
  (define data (interpret-lines lines mask settings))
  (find-columns data))

;; initialize-vector: returns a vector where every element is false

(check-expect (initialize-vector (vector 0 0 0 0 0)) '#(#f #f #f #f #f))

(define (initialize-vector v)
  (define v-lenght (vector-length v))
  (for ([i v-lenght])
    (vector-set! v i #f))
  v)

;; interpret-lines: returns a vector of binary numbers where 0 represents a delimiter
;; and 1 is anything else
;; lines    -> the lines of the fwf
;; mask     -> a Vector with max-line-length + 1 elements
;; settings -> a hashtable of settings

(check-expect (interpret-lines '("11\t 34"
                                "11 233") (initialize-vector (make-vector 6)) SETTINGS) '#(#t #t #f #t #t #t))

(define (interpret-lines lines mask settings)
  (define lineLength (length lines))
  (define check-lines-length (hash-ref settings 'check-lines))
  
  (define increment (if (< lineLength check-lines-length)
                        lineLength
                        check-lines-length))
  
  (for ([i increment])
    (define line-chars (string->charList (list-ref lines i)))
    (define line-chars-length (length line-chars))
    (for ([j line-chars-length])
      (vector-set! mask
                   j
                   (or (vector-ref mask j)
                       (not (isDelimiter? (list-ref line-chars j) (hash-ref settings 'delimiters)))))))
  mask)

;; find-columns: given a mask finds all locations where 0 changes to one. returns a list of
;; indices that correspond to the starting location of the column
;; data-mask -> vector of binary numbers
(check-expect (find-columns '#(#f #t #t #t #t #f #f #f #t #t #t #f #t)) '(1 8 12))

(define (find-columns data-mask)
  (define maskLength (vector-length data-mask))
  (define previous #f)
  (define hold '())
  (for ([i maskLength])
    (cond [(and (boolean=? #f previous) (boolean=? #t (vector-ref data-mask i)))
           (set! hold (cons i hold))
           (set! previous (vector-ref data-mask i))]
          [else (set! previous (vector-ref data-mask i))]))
  (reverse hold))

;; delimiter->binary: returns 0 if the given character is a delimiter else returns 1

(define (delimiter->binary char delimiters)
  (if (isDelimiter? char delimiters)
      0
      1))

;; isDelimiter: returns true if the given item is in DELIMITERS
(check-expect (isDelimiter? 2 '(" " "\t")) #f)
(check-expect (isDelimiter? " " '(" " "\t")) #t)
(check-expect (isDelimiter? "\t" '(" " "\t")) #t)


(define (isDelimiter? item delimiters)
  (not (boolean? (member item delimiters)))) ;;member returns a list if the item is in delimiters and false if it is not


;; parse-fwf: using the given colspecs returns final data

(define (parse-fwf lines colspecs settings)
  (display colspecs)
  (display "\n")
  (define lineLength (length lines))
  (define hold '())
  (for ([i lineLength])
    (define current-line (list-ref lines i))
    (if (< (string-length current-line) (last colspecs)) ;; makes sure line has enough characters
        (void)
        (set! hold (cons (parse-line current-line colspecs) hold))))
  (reverse hold))

;; parse-line: parses a single line

(check-expect (parse-line "Hello My Friend" '(0 6 9)) '#("Hello " "My " "Friend"))

(define (parse-line line colspecs)
  (define number-of-columns (length colspecs))
  (define new-vector (make-vector number-of-columns))
  (for ([i (- number-of-columns 1)])
    (define startpoint (list-ref colspecs i))
    (define endpoint (list-ref colspecs (+ i 1)))
    (vector-set! new-vector i (substring line startpoint endpoint)))
  (vector-set! new-vector (- number-of-columns 1) (substring line (last colspecs)))
  new-vector)










;; Things Racket Doesn't have but probably should

;; remove-first-n: removes first n items from the front of the given list
;; n : natural number: how many items to remove

(check-expect (remove-first-n '(1 2 3) 2) '(3))
(check-error (remove-first-n '(1 2 3) -2) "remove-first-n: n must be greater than zero")

(define (remove-first-n list n)
  (cond [(> 0 n) (error "remove-first-n: n must be greater than zero")]
        [(or (= n 0) (empty? list)) list]
        [else (remove-first-n (rest list) (- n 1))]))

;; string->charList: converts the given string to the character list

(check-expect (string->charList "abc") '("a" "b" "c"))

(define (string->charList str)
  (string->charList-acc str '()))

;; string->charList-acc: accumulator or string->charList

(define (string->charList-acc str hold)
  (cond [(string=? str "") hold]
        [else (string->charList-acc (substring str 1 (string-length str)) (append hold (list (substring str 0 1))))]))


;; Random Helper Functions

;; max-length: returns the length of the longest string in the given list of strings
;; los -> List of Strings

(check-expect (longest-string '("a" "aa" "aaa" "aa" "aaaa")) 4)
(check-expect (longest-string '("123456789" "1234" "12345678910")) 11)

(define (longest-string los)
  (foldr (λ (x y) (define l (string-length x)) ;; length of next string from list
                  (if (> l y)  ;; compares current string to legth of the old one
                      l
                      y))
         0 los))





(read-fwf TEST-FILE "infer" 3)
(test)

