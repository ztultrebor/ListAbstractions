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


(define (andmap-2 pred lst)
  ; [X -> Boolean] [ListOf X] -> Boolean
  ; inner workings of andmap function
  (or
   (empty? lst)
   (and
    (pred (first lst))
    (andmap-2 pred (rest lst)))))


(define (ormap-2 pred lst)
  ; [X -> Boolean] [ListOf X] -> Boolean
  ; inner workings of ormap function
  (and
   (not (empty? lst))
   (or
    (pred (first lst))
    (ormap-2 pred (rest lst)))))



; =====================
; checks
(define lst '(0 1 2 3 4 5 6 7 8 9))
(define gre0? (lambda (x) (>= x 0)))
(define l0? (lambda (x) (< x 0)))
(check-expect (map-2 sqr lst) (map sqr lst))
(check-expect (andmap-2 even? lst) (andmap even? lst))
(check-expect (andmap-2 gre0? lst) (andmap gre0? lst))
(check-expect (ormap-2 even? lst) (ormap even? lst))
(check-expect (ormap-2 l0? lst) (ormap l0? lst))