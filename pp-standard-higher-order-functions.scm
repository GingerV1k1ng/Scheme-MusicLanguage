; The functions in this file are 'standard PP higher-order functions'.
; Feel free to use these in PP programming contexts, such as in the first PP miniproject.
; For documentation, see the PP slides and videos.
; /KN

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (find-in-list pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-in-list pred (cdr lst)))))

(define (make-selector-function n)
  (lambda (lst) (list-ref lst (- n 1))))

(define (filter pred lst)
  (reverse (filter-help pred lst '())))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res)))
        (else 
           (filter-help pred (cdr lst)  res))))

(define (reduce-right f lst)
  (if (null? (cdr lst))
      (car lst)
      (f (car lst) 
         (reduce-right f (cdr lst)))))

(define (reduce-left f lst)
  (reduce-help-left f (cdr lst) (car lst)))

(define (reduce-help-left f lst res)
  (if (null? lst)
      res
      (reduce-help-left f (cdr lst) (f res (car lst)))))

(define (accumulate-right f init lst)
  (if (null? lst)
      init
      (f (car lst) (accumulate-right f init (cdr lst)))))

(define (zip z lst1 lst2)
  (if (null? lst1)
      '()
      (cons 
        (z (car lst1) (car lst2))
        (zip z (cdr lst1) (cdr lst2)))))

(define (curry2 f)
  (lambda(x)
    (lambda(y)
      (f x y))))

(define (curry3 f)
  (lambda(x)
    (lambda(y)
      (lambda(z)
       (f x y z)))))

(define (uncurry2 f)
  (lambda (x y)
    ((f x) y)))

(define (uncurry3 f)
  (lambda (x y z)
    (((f x) y) z)))