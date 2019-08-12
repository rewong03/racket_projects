;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |go-client |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)
(require "go.rkt")
(provide (all-defined-out))

;game: l (list of all pieces) c (color of player) s (boolean; whether player has passed) p (points)
(define-struct game (l c s p))

(define (change-l struct l)
  (make-game l
             (game-c struct)
             (game-s struct)
             (game-p struct)))
(define (change-s struct s)
  (make-game (game-l struct)
             (game-c struct)
             s
             (game-p struct)))
(define (change-p struct p)
  (make-game (game-l struct)
             (game-c struct)
             (game-s struct)
             p))

(define (new-draw-help struct)
  (draw-h (game-l struct)))

(define (whose-turn? game)
  (cond [(equal? (game-s game) "choose")
         (game-c game)]
        [(and (equal? (game-s game) #t)
              (equal? "b" (game-c game)))
         "Black Passed"]
        [(and (equal? (game-s game) #t)
              (equal? "w" (game-c game)))
         "White Passed"]
        [(empty? (game-l game))
         "b"]
        [else (opposite-color-list game)]))

(define (whose-turn-display game)
  (cond [(equal? (whose-turn? game) "b")
         (text "Black's Turn" 20 "black")]
        [else (text "White's Turn" 20 "black")]))

(define (new-draw-h struct)
  (beside (new-draw-help struct)
          (whose-turn-display struct)))                                

(define (opposite-color game)
  (if (equal? (game-c game) "w")
      "b" "w"))
(define (opposite-color-list game)
  (if (equal? (p-c (first (game-l game))) "b")
      "w" "b"))

(define (your-turn? game)
  (cond [(equal? "choose" (game-s game))
         true]
        [(and (empty? (game-l game))
              (equal? "b" (game-c game)))
         true]
        [(empty? (game-l game))
         false]
        [(equal? (p-c (first (game-l game))) (opposite-color game))
         true]
        [else false]))

(define (new-mouse-h struct x y event)
  (cond [(not (your-turn? struct))
         struct]
        [(suicide-rule (game-l struct) x y event)
         struct]
        [(mouse=? event "button-down")
         (make-package (change-s struct #f)
                       (list (game-c struct) (list (posn-x (xy->coord x y))
                                                   (posn-y (xy->coord x y)))))]
        [else struct]))

(define (msg->piece msg)
  (make-p (first (first (rest msg)))
          (first (rest (first (rest msg))))
          (first msg)))

(define (key-h struct k)
  (cond [(key=? k " ")
         (make-package (change-s struct #t)
                       (list "pass" (game-c struct)))]
        [else struct]))

(define (send-game-over? struct msg)
  (if (and (equal? (game-s struct) #t)
           (equal? msg (list "pass" (opposite-color struct))))
      true false))

(define (tick-h struct)
  (if (equal? "game-over" (game-s struct))
      (make-package (change-p struct (find-scores struct))
                    (list (find-scores struct) (game-c struct)))
      struct))

(define (receive-h struct msg)
  (cond [(send-game-over? struct msg)
         (make-package struct
                       "game-over")]
        [(equal? msg "game-over")
         (change-s struct msg)]
        [(and (number? (first msg))
              (equal? (game-c struct) (first (rest msg))))
         struct]
        [(and (number? (first msg))
              (not (equal? (game-c struct) (first (rest msg)))))
         (change-c struct (list (game-s struct)
                                (first msg)))]
        [(and (equal? (first msg) "pass")
              (not (equal? (game-c struct) (first (rest msg)))))
         (change-s struct "choose")]
        [(equal? (first msg) "pass")
         struct]
        [(results-in-capture? (msg->piece msg) (game-l struct))
         (change-l struct
                   (list* (msg->piece msg)
                          (remove-captured-chain (find-adjacent-chain (msg->piece msg) (game-l struct))
                                                 (game-l struct))))]         
        [else (change-l struct
                        (list* (msg->piece msg)
                               (game-l struct)))]))

(define (remove-posn-from-list p l)
  (cond [(empty? l)
         empty]
        [(equal? p (first l))
         (rest l)]
        [else (list* (first l)
                     (remove-posn-from-list p (rest l)))]))

(define (remove-list-p-from-list l1 l2)
  (cond [(empty? l1)
         empty]
        [else (remove-list-p-from-list (rest l1)
                                       (remove-posn-from-list (first l1) l2))]))

(define (find-scores-help game x1 y1 x2 y2 l)
  (cond [(equal? y1 (+ 1 y2))
         (game-p game)]
        [(equal? x1 (+ x2 1))
         (find-scores-help game 0 (+ 1 y1) x2 y2 l)]
        [(member? (make-posn x1 y1) l)
         (find-scores-help game (+ x1 1) y1 x2 y2 l)]
        [(and (is-valid-points? (make-posn x1 y1) (game-l game))
              (equal? (whose-points? (make-posn x1 y1) (game-l game)) (game-c game)))
         (find-scores-help (change-p game (+ (length (first (find-territory (list (make-posn x1 y1))
                                                                            (game-l game))))
                                             (game-p game)))
                           (+ x1 1) y1 x2 y2
                           (append (first (find-territory (list (make-posn x1 y1))
                                                          (game-l game)))
                                   l))]
        [else (find-scores-help game
                                (+ x1 1) y1 x2 y2 l)]))
(define (find-scores game)
  (find-scores-help game 0 0 BOARD_SIZE BOARD_SIZE empty))

(define (stop-when? struct)
  (if (equal? (length (game-p struct)) 2)
      true false))
(define (last-scene struct)
  (overlay (above (text (string-append "Your Score: "
                                       (number->string (first (game-p struct))))
                        30 "black")
                  (text (string-append "Opponent's Score: "
                                       (number->string (first (rest (game-p struct)))))
                        30 "black"))
           (empty-scene 500 500)))

(define test (make-game (list
                         (make-p 1 2 "b")
                         (make-p 3 5 "w")
                         (make-p 2 3 "b")
                         (make-p 4 5 "w")
                         (make-p 3 3 "b")
                         (make-p 6 5 "w")
                         (make-p 4 2 "b")
                         (make-p 5 4 "w")
                         (make-p 4 1 "b")
                         (make-p 3 0 "b")
                         (make-p 1 1 "b")
                         (make-p 2 0 "b")
                         (make-p 5 5 "b")) "b" #f 0))

(define (run-program m)
  (big-bang m
            (on-draw new-draw-h)
            (on-mouse new-mouse-h)
            (on-receive receive-h)
            (on-tick tick-h)
            (state #f)
            (close-on-stop #t)
            (register LOCALHOST)
            (on-key key-h)
            (stop-when stop-when? last-scene)))

(launch-many-worlds (run-program (make-game empty "b" #f empty))
                   (run-program (make-game empty "w" #f empty)))














