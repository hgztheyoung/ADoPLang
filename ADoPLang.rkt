#lang Racket
(require (for-syntax syntax/parse))
(provide (except-out (all-defined-out)
                     loop))
(define-syntax skip
  (syntax-rules ()
    [(skip) (void)]))

(define-syntax abort
  (syntax-rules ()
    [(abort) (error "aborted")]))

(define-syntax :=
  (syntax-rules ()
    [(_ (x ...) (y ...))
     (set!-values (x ...) (values y ...))]
    [(_ x y)
     (set! x y)]))

(define-syntax def
  (syntax-rules ()
    [(_ (x ...) (y ...))
     (define-values (x ...) (values y ...))]
    [(_ x y)
     (define x y)]))

;p is the count of unencountered true branches
(define-syntax condp
  (syntax-rules (else)    
    [(_ p [(else e1 e2 ...)]) (begin e1 e2 ...)]
    [(_ p [(e0 e1 e2 ...)]) (when e0 (begin e1 e2 ...))]
    [(_ p [(e0 e1 e2 ...) c1 c2 ...])
     (if e0
         (if (< (random) (/ 1 p)) (begin e1 e2 ...) (condp (- p 1) [c1 c2 ...]))
         (condp p [c1 c2 ...]))]))

(define-syntax iffi
  (syntax-rules ()
    [(_  (pred conseq) ...)
     (let ([tcount (count (lambda (x) (eq? x #t)) (list pred ...))])
       (if (eq? tcount 0)
           (abort)
           (condp tcount [(pred conseq) ...])))]))

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
