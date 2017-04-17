#lang racket
(require  "./ADopLang.rkt")

(define (f X Y)
  (def (x y) (X Y))
  (dood ((> x y) (:= x (- x y)))
        ((< x y) (:= y (- y x))))
  (print x))

(define (g X Y)
  (def (x y u v) (X Y Y X))
  (dood ((> x y) (:= (x v) ((- x y) (+ v u))))
        ((> y x) (:= (y u) ((- y x) (+ u v)))))
  (println (/ (+ x y) 2))
  (println (/ (+ u v) 2)))

(g 24 36)

;(iffi ((#f "aborts"))
;(dood ((#t "loop forever"))

(define (s4 Q1 Q2 Q3 Q4)
  (def (q1 q2 q3 q4) (Q1 Q2 Q3 Q4))
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

(let ([x 1])
  (dood ((neq? x 100) (begin (q) (:= x (+ x 1)))))
  (println ""))

(begin (def x 1) (dood ((neq? x 10) (:= x (+ x 1)))) (print x))

(condp 2
       [(#t 1)
        (else 2)])

;strange is of type 50% to be int or 25% to be bool or 25% to be string
;the above is just some type illusion, there's no explicit type in ADoPLang
(def strange
  (iffi
   (#t 1)
   (#t (iffi (#t #t)
             (#t "213")))))

