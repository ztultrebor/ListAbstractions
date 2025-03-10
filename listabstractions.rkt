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


(define (map-3 f lst1 lst2)
  ; [X Y -> Z] [ListOf X] [ListOf Y] -> [ListOf Z]
  ; inner workings of map abstraction for 2 lists simultaneously
  (local ((define (map l1 l2)
            (cond
              [(empty? l1) '()]
              [else
               (cons (f (first l1) (first l2)) (map (rest l1) (rest l2)))])))
    ; - IN -
    (if (not (= (length lst1) (length lst2)))
        (error "all lists must have same size")
        (map lst1 lst2))))


(define (andmap-2 pred lst)
  ; [X -> Boolean] [ListOf X] -> Boolean
  ; inner workings of andmap abstraction
  (or
   (empty? lst)
   (and
    (pred (first lst))
    (andmap-2 pred (rest lst)))))


(define (andmap-3 pred lst1 lst2)
  ; [X Y -> Boolean] [ListOf X] [ListOf Y] -> Boolean
  ; inner workings of andmap abstraction for 2 lists simultaneously
  (local ((define (andmap l1 l2)
            (or
             (empty? l1)
             (and
              (pred (first l1) (first l2))
              (andmap (rest l1) (rest l1))))))
    ; - IN -
    (if (not (= (length lst1) (length lst2)))
        (error "all lists must have same size")
        (andmap lst1 lst2))))


(define (ormap-2 pred lst)
  ; [X -> Boolean] [ListOf X] -> Boolean
  ; inner workings of ormap abstraction
  (and
   (not (empty? lst))
   (or
    (pred (first lst))
    (ormap-2 pred (rest lst)))))


(define (ormap-3 pred lst1 lst2)
  ; [X Y -> Boolean] [ListOf X] [ListOf Y] -> Boolean
  ; inner workings of ormap abstraction for 2 lists simultaneously
  (local ((define (ormap l1 l2)
            (and
             (not (empty? l1))
             (or
              (pred (first l1) (first l2))
              (ormap (rest l1) (rest l1))))))
    ; - IN -
    (if (not (= (length lst1) (length lst2)))
        (error "all lists must have same size")
        (ormap lst1 lst2))))


(define (filter-2 pred lst)
  ; [X -> Boolean] [ListOf X] -> [ListOf X]
  ; inner workings of filter abstraction
  (cond
    [(empty? lst) '()]
    [(pred (first lst)) (cons (first lst) (filter-2 pred (rest lst)))]
    [else (filter-2 pred (rest lst))]))
  

(define (foldr-2 f default lst)
  ; [X Y -> Y] Y -> Y
  ; inner workings of foldr abstraction
  (cond
    [(empty? lst) default]
    [else (f (first lst) (foldr-2 f default (rest lst)))]))


(define (foldr-3 f default lst1 lst2)
  ; [X Y Z -> Z] [ListOf X] [ListOf Y] -> Z
  ; inner workings of foldr abstraction for 2 lists simultaneously
  (local ((define (foldr l1 l2)
            (cond
              [(empty? l1) default]
              [else (f (first l1) (first l2) (foldr (rest l1) (rest l2)))])))
    ; - IN -
    (if (not (= (length lst1) (length lst2)))
        (error "all lists must have same size")
        (foldr lst1 lst2))))


(define (foldl-2 f default lst)
  ; [X Y -> Y] Y -> Y
  ; inner workings of foldl abstraction
  (local ((define (foldl lst acc)
            (cond
              [(empty? lst) acc]
              [else (foldl (rest lst) (f (first lst) acc))])))
    ; - IN -
    (foldl lst default)))


(define (foldl-3 f default lst1 lst2)
  ; [X Y Z -> Z] [ListOf X] [ListOf Y] -> Z
  ; inner workings of foldl abstraction for 2 lists simultaneously
  (local ((define (foldl l1 l2 acc)
            (cond
              [(empty? l1) acc]
              [else
               (foldl (rest l1) (rest l2) (f (first l1) (first l2) acc))])))
    ; - IN -
    (if (not (= (length lst1) (length lst2)))
        (error "all lists must have same size")
        (foldl lst1 lst2 default))))


; =====================
; checks
(define lst '(0 1 2 3 4 5 6 7 8 9))
(define lst2 '(0 1 2 3 4 5 6 7 8 9 0))
(define lst3 '(1 2 3 4 5 6 7 8 9 0))
(define lst4 '("" "." ".." "..." "...." "....."))
(define gre0? (lambda (x) (>= x 0)))
(define l0? (lambda (x) (< x 0)))
(define dots (lambda (x y) (+ (string-length x) y)))
(define max+ (lambda (x y z) (+ (if (> x y) x y) z)))
(check-expect (map-2 sqr lst) (map sqr lst))
(check-expect (map-3 expt lst lst)(map expt lst lst))
(check-error (map-3 expt lst lst2) "all lists must have same size")
(check-expect (andmap-2 even? lst) (andmap even? lst))
(check-expect (andmap-2 gre0? lst) (andmap gre0? lst))
(check-expect (andmap-3 < lst lst3) (andmap < lst lst3))
(check-error (andmap-3 < lst lst2) "all lists must have same size")
(check-expect (ormap-2 even? lst) (ormap even? lst))
(check-expect (ormap-2 l0? lst) (ormap l0? lst))
(check-expect (ormap-3 < lst lst3) (ormap < lst lst3))
(check-error (ormap-3 < lst lst2) "all lists must have same size")
(check-expect (filter-2 even? lst) (filter even? lst))
(check-expect (filter-2 l0? lst) (filter l0? lst))
(check-expect (foldr-2 dots 0 lst4) (foldr dots 0 lst4))
(check-expect (foldr-3 max+ 0 lst lst3) (foldr max+ 0 lst lst3))
(check-error (foldr-3 max+ 0 lst lst2) "all lists must have same size")
(check-expect (foldl-2 dots 0 lst4) (foldl dots 0 lst4))
(check-expect (foldl-3 max+ 0 lst lst3) (foldl max+ 0 lst lst3))
(check-error (foldl-3 max+ 0 lst lst2) "all lists must have same size")