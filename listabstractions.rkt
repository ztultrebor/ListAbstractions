;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname listabstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



; =======================
; functions

(define (map-2 f lst)
  ; [X -> Y] [ListOf X] -> [ListOf Y]
  ; inner workings of map abstraction
  (cond
    [(empty? lst) '()]
    [else (cons (f (first lst)) (map-2 f (rest lst)))]))



; =====================
; checks
(define lst '(0 1 2 3 4 5 6 7 8 9))
(check-expect (map-2 sqr lst) (map sqr lst))