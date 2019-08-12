#lang racket
#|
Version: 1.0.0
Bare-bones server code written by Doc Mo.
Just repeats message it receives to all clients.
|#

(require rackunit)
(require 2htdp/universe)

(struct server-state (worlds bang-state) #:transparent)

(define STARTING-STATE #f)

; on-message-a: server-state world mail-sexp -> server-state
; purpose: update the server state when message is received

(define (on-message-a u-state sender-world msg)
  (struct-copy server-state u-state [bang-state msg]))


; on-message-b: server-state world mail-sexp -> list-of-mail
; purpose: produce messages for every connected world

(define (on-message-b u-state sender-world msg)
  (local [(define (mail-maker one-world)
            (make-mail one-world msg))]
    (map mail-maker (server-state-worlds u-state))))

; message-h: server-state world mail-sexp -> bundle
(define (message-h u-state sender-world msg)
  (make-bundle (on-message-a u-state sender-world msg)
               (on-message-b u-state sender-world msg)
               empty))
        
(define (new-h u-state iworld)
  (cond [(not (member iworld (server-state-worlds u-state)))
         (struct-copy server-state
                      u-state
                      [worlds (list* iworld (server-state-worlds u-state))])]
        [else ;already on list
         u-state]))

(define (disconnect-h u-state iworld)
  (make-bundle
   (struct-copy server-state
                u-state
                [worlds (remove iworld (server-state-worlds u-state))])
   empty
   (list iworld)))


(define starting-universe-model
  (server-state empty STARTING-STATE))

(universe starting-universe-model
          (on-new new-h)
          (on-disconnect disconnect-h)
          (on-msg message-h))

