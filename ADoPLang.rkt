#lang Racket
(require (for-syntax syntax/parse))
(provide (except-out (all-defined-out)
                     loop))
(define-syntax skip
  (syntax-rules ()
    [(skip) (void)]))

(define-syntax seq
  (syntax-rules ()
    [(seq e0 ...) (begin e0 ...)]))

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

(define-syntax condpOrigin
  (syntax-rules ()
    [(_ () ()) (void)]
    [(_ (e0 e1 ...) (c0 c1 ...))
     (let* ([l (vector (cons e0 (lambda () c0)) (cons e1 (lambda () c1)) ...)]
            [t (vector-filter-not (lambda (p) (false? (car p))) l)]
            [tlen (vector-length t)])
       (if (= tlen 0)
           (skip)
           ((cdr (vector-ref t (random tlen))))))]))

(define-syntax iffi
  (syntax-rules (else)
    [(_ (e0 e1 e2 ...)) (if e0 (seq e1 e2 ...) (skip))]
    [(_ (e0 e1 e2 ...) ...)
     (condpOrigin (e0 ...) ((seq e1 e2 ...) ...))]))



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
