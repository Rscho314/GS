#lang racket

(require redex
         redex/tut-subst)

;untyped lambda calculus
(define-language L
  (E N F A)
  (N variable-not-otherwise-mentioned)
  (F (λ (N) E))
  (A (E E)))

;add numbers
(define-extended-language LN L
  (E ....
     (+ E E) ;test addition
     number))

;add execution context and values
(define-extended-language CV LN
  (p (E ...))
  (P (E ... C E ...))
  (C (V C)
     (C E)
     (+ V ... C E ...)
     hole)
  (V (λ (N) E)
     number))

;lift metafunctions to racket
(define-metafunction CV
  Σ : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])

(define-metafunction CV
  subst : N V E -> E
  [(subst N V E)
   ,(subst/proc N? (list (term N)) (list (term V)) (term E))])
(define N? (redex-match CV N))

;reduction relations
(define red
  (reduction-relation
   CV
   #:domain p
   (--> (in-hole P ((λ (N) E) V))
        (in-hole P (subst N V E))
        "β")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "+")))

;test
(traces
   red
   (term (((λ (x) (+ x 1)) 1))))