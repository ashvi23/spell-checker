#lang racket

; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2019                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains simple dictionary definition
(require "test-dictionary.rkt")
(require racket/trace)
;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***
; create bit vector
; takes in the hash function list and the dictionary
; hashes each entry in the dictionary adn saves it to bitvector
;(define createbv
 ; (lambda (hashl dict)
     ; hash(createbv (hashfl cdr dict))
  ;  (if(null? hashl)
   ;    '()
    ;   (append hashl (createbv hashl (cdr dict)))
    ;)))

; compares the word's list of hash values to the bitvector
; takes in a list of hashed values and bitvector

;(define comparewords
  ;(lambda (whashl bitvector)

  ;  )
 ; )
;(define bitvec '())

(define createbv
  (lambda (hashlist dict)
   (cond((null? hashlist) '())
        (else 
        (cons(map(car hashlist)dict) (createbv (cdr hashlist)dict))
       )
       )
    )
  )
    
 (trace createbv)

(define wordhash
  (lambda (hashlist word)
    (cond ((null? hashlist) '())
          (else
           (cons ((car hashlist) word)
                 (wordhash (cdr hashlist) word)
                 )
           )
         
    )))
(trace wordhash)

(define compare
  (lambda (bv wordhashlist)
   (if(null? wordhashlist)
      '#t
      ;'()
    (cond [(member(car wordhashlist)bv)(compare(cdr wordhashlist)bv)]
    (else '#f)
          )
   ))
)
;; -----------------------------------------------------
;; KEY FUNCTION
 ; (define key 5413)
      ;(for([i (in-string w)]
            ; add method to read from right to left
         ;29 * key + ctv(c)
          ; (set! key(+ (* 29 key) (ctv c)))))
(define key
  (lambda (w)
   ; (cond
    ;  [(empty? w)empty]
     ;[else
      ;key(+(* 29 key) (ctv cons reverse'(w)))] )
     (if (null? w)
        5413
        (+
         (*
          29
          (key (cdr w))
          )
         (ctv (car w))
             
         )
        )
    )
  )

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 111037761665
;;   (key '(m a y))           = 132038724
;;   (key '(t r e e f r o g)) = 2707963878412931

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
       (modulo (key w) size)
      )
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
    (floor
      (*
       size
         (-
          (*
            A
            (key w)
            )
          (floor
           (*
            A
            (key w)
            )
           )
          )
         )
))))
;(trace gen-hash-multiplication-method)

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89989))
(define hash-3 (gen-hash-multiplication-method 700426))
(define hash-4 (gen-hash-multiplication-method 952))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;; (hash-1 '(h e l l o))        ==> 26303
;; (hash-1 '(m a y))            ==> 19711
;; (hash-1 '(t r e e f r o g))  ==> 3010
;;
;; (hash-2 '(h e l l o))        ==> 64598
;; (hash-2 '(m a y))            ==> 24861
;; (hash-2 '(t r e e f r o g))  ==> 23090
;;
;; (hash-3 '(h e l l o))        ==> 313800.0
;; (hash-3 '(m a y))            ==> 317136.0
;; (hash-3 '(t r e e f r o g))  ==> 525319.0
;;
;; (hash-4 '(h e l l o))        ==> 426.0
;; (hash-4 '(m a y))            ==> 431.0
;; (hash-4 '(t r e e f r o g))  ==> 714.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR
;(define bv(createbv hashfl-1 dict))
(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda (w)
      
    (compare  (createbv hashfunctionlist dict)
      (wordhash hashfunctionlist w)

      )
      )
    )
  )

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS
(define bv(createbv hashfl-1 dictionary))
(define bv2(createbv hashfl-2 dictionary))
(define bv3(createbv hashfl-3 dictionary))


(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #f
