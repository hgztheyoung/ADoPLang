#lang racket
(require (for-syntax syntax/parse))

(define-syntax skip
  (lambda (stx)
    (syntax-parse stx
      [(skip) #'(void)])))

(define-syntax abort
  (lambda (stx)
    (syntax-parse stx
      [(abort) #'(error "aborted")])))

(define-syntax :=
  (lambda (stx)
    (syntax-parse stx      
      [(_ (x ...) (y ...))
       #'(set!-values (x ...) (values y ...))]
      [(_ x y)
       #'(set! x y)])))

(define-syntax init
  (lambda (stx)
    (syntax-parse stx      
      [(_ (x ...) (y ...))
       #'(define-values (x ...) (values y ...))]
      [(_ x y)
       #'(define x y)])))

;p is the count of unencountered true branches
(define-syntax condp
  (syntax-rules ()    
    [(_ p [(else e1 e2 ...)]) (begin e1 e2 ...)]
    [(_ p [(e0 e1 e2 ...)]) (if e0 (begin e1 e2 ...))]
    [(_ p [(e0 e1 e2 ...) c1 c2 ...])
     (if e0
         (if (< (random) (/ 1 p)) (begin e1 e2 ...) (condp (- p 1) [c1 c2 ...]))
         (condp p [c1 c2 ...]))]))

(define-syntax iffi
  (lambda (stx)
    (syntax-parse stx
      [(_  (pred conseq) ...)
       #'(let ([tcount (count (lambda (x) (eq? x #t)) (list pred ...))])
           (if (eq? tcount 0)
               (abort)
               (condp tcount [(pred conseq) ...])))])))

(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break (datum->syntax #'k 'break)])
         #'(call/cc
            (lambda (break)
              (let f () e ... (f)))))])))

(define-syntax dood
  (syntax-rules ()
    [(dood (pred conseq) ...)
     (loop
      (let ([tcount (count (lambda (x) (eq? x #t)) (list pred ...))])
        (if (eq? tcount 0)
            (break)
            (iffi (pred conseq) ...))))]))

(define-syntax neq?
  (syntax-rules ()
   [(neq? x y) (not (eq? x y))]))


(define (f X Y)
  (init (x y) (X Y))
  (dood ((> x y) (:= x (- x y)))
        ((< x y) (:= y (- y x))))
  (print x))

(define (g X Y)
  (init (x y u v) (X Y Y X))
  (dood ((> x y) (:= (x v) ((- x y) (+ v u))))
        ((> y x) (:= (y u) ((- y x) (+ u v)))))
  (println (/ (+ x y) 2))
  (println (/ (+ u v) 2)))

(g 24 36)

;(iffi ((#f "aborts"))
;(dood ((#t "loop forever"))

(define (s4 Q1 Q2 Q3 Q4)
  (init (q1 q2 q3 q4) (Q1 Q2 Q3 Q4))
  (dood ((> q1 q2) (:= (q1 q2) (q2 q1)))
        ((> q2 q3) (:= (q3 q2) (q2 q3)))
        ((> q3 q4) (:= (q3 q4) (q4 q3))))
  (print (list q1 q2 q3 q4)))
(s4 3 1 4 2)

(define (q)
  (print
   (iffi
    (#t 1)
    (#f 2)
    (#t 3)
    (#f 4))))

(let ([i 0])
  (loop
   (q)
   (if (eq? i 100)
       (break)
       (set! i (+ i 1)))))

(begin (init x 1) (dood ((neq? x 10) (:= x (+ x 1)))) (print x))