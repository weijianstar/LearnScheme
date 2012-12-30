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
    