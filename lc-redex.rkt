#lang racket

(require redex)

;untyped lambda calculus
(define-language L
  (E ::= N
         (λ (N) E)
         (E E))
  (N ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (N) E #:refers-to N))
 
(default-language L)

(define-extended-language LC L
  (C ::=
     hole
     (lambda (N) C)
     (C E)
     (E C)))

(define red
  (reduction-relation
   LC
   #:domain E
   (--> (in-hole C ((λ (N) E_1) E_2))
        (in-hole C (substitute E_1 N E_2))
        "β")))

(test-->>
 red
 (term ((λ (f) ((λ (a) (f a)) b)) a))
 (term (a b))) ;α conversion works
(test-results)