;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4exerise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;exercise 117
(define (sum list-of-amounts)
  (cond
    [(empty? list-of-amounts) 0]
    [else (+ (sum (rest list-of-amounts)) (first list-of-amounts))]))

;exercise 118
(define (pos? list-of-amounts)
  (cond
    [(empty? list-of-amounts) true]
    [else (and (number? (first list-of-amounts)) (pos? (rest list-of-amounts)))]))
;exercise 119
(define (all-true? list-of-booleans)
  (cond
    [(empty? list-of-booleans) true]
    [else (and (boolean? (first list-of-booleans)) (all-true? (rest list-of-booleans)))]))

(define (one-true? list-of-booleans)
  (cond
    [(empty? list-of-booleans) false]
    [else (or (boolean? (first list-of-booleans)) (one-true? (rest list-of-booleans)))]))

;exercise 120
(define (juxtapose list-of-strings)
  (cond
  [(empty? list-of-strings) ""]
  [else (string-append (first list-of-strings) (juxtapose (rest list-of-strings)))]))

;exercise122
(define (how-many alot)
  (cond
   [(empty? alot) 0]
   [else (+ (sum (rest alot)) 1)]))

(define (sum-alot alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

(define (check-average alot)
  (cond
    [(empty? alot) false]
    [else true]))

(define (average alot)
  (cond
    [(check-average alot) (/ (sum-alot alot) (how-many alot))]
    [else (error "empty list")]))

(define (sum-anelot anelot)
  (cond
    [(empty? (rest anelot)) (first anelot)]
    [(cons? (rest anelot)) (+ (first anelot) (sum-anelot (rest anelot)))]))

;exercise 128
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

;exercise 129
(define (add value x)
  (+ value x))

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(= 1 n) x]
    [(positive? n) (add(multiply (sub1 n) x) x)]))
;exercise 132
(define-struct layer (color doll))

; RD -> Number
; how many dolls are a part of an-rd
(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd) (+ (depth (layer-doll an-rd)) 1)]))


(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (string-append (colors (layer-doll an-rd)) "," (layer-color an-rd))]))

;exercise 133
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (inner (layer-doll an-rd))]))

;exerise 138 and 139

; List-of-numbers -> List-of-numbers
; compute the weekly wages for all given weekly hours
(define (wage* alon)
  (cond
    [(empty? alon) empty]
    [(> (first alon) 100) (error "hours > 100")]
    [else (cons (wage (first alon)) (wage* (rest alon)))]))
 
; Number -> Number
; compute the wage for h hours of work
(define (wage h)
  (* 14 h))

;exercise 140
(define (convertFC fahrenheit)
  (cond
    [(empty? fahrenheit) empty]
    [else (cons (f-to-c (first fahrenheit)) (convertFC(rest fahrenheit)))]))

(define (f-to-c fahrenheit)
  (/ (- fahrenheit 32) 1.8))


;exercise 147
(define-struct phone (area switch four))

(define (replace list-of-phones)
  (cond
    [(empty? list-of-phones) empty]
    [(= 713 (phone-area (first list-of-phones))) (cons (make-phone 281 (phone-switch (first list-of-phones)) (phone-four (first list-of-phones))) (replace (rest list-of-phones)))]
    [else (cons (make-phone (phone-area (first list-of-phones)) (phone-switch (first list-of-phones)) (phone-four (first list-of-phones))) (replace (rest list-of-phones)))]))


;exercise 149
(define (convert-lines-to-string lines)
  (cond
    [(empty? lines) empty]
    [(empty? (rest lines)) (first lines)]
    [(cons? lines) (string-append (first lines) "\n" (convert-lines-to-string (rest lines)))]))

;exercise 164
(define-struct gp (name score))

(define (sort players)
  (cond
    [(empty? players) empty]
    [(cons? players) (insert (first players) (sort (rest players)))]))

(define (insert player players)
  (cond
    [(empty? players) (cons player empty)]
    [else (if (>= (gp-score player) (gp-score (first players)))
          (cons player players)
          (cons (first players) (insert player (rest players))))]))
          
;exercise165
(define (insertv2 n alon)
  (cond
    [(empty? alon) (cons n empty)]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insertv2 n (rest alon))))]))
(define (sort-> alon)
  (cond
    [(empty? alon) empty]
    [(cons? alon) (insertv2 (first alon) (sort-> (rest alon)))]))


(define (search-sort n alon)
  (cond
    [(empty? alon) false]
    [else (or (= (first alon) n) (search n (rest alon)))]))

(define (search n alon)
  (search-sort n (sort-> alon)))

                                         