;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)

;struct definitions
;game: score (num) dir (posn) p-posn (list of posns) a-posn (posn)
(define-struct game (score dir p-posn a-posn))

(define (change-score int struct)
  (make-game int
             (game-dir struct)
             (game-p-posn struct)
             (game-a-posn struct)))
(check-expect (change-score 1 (make-game 0 (make-posn 1 2) empty (make-posn 2 3)))
              (make-game 1 (make-posn 1 2) empty (make-posn 2 3)))

(define (change-dir posn struct)
  (make-game (game-score struct)
             posn
             (game-p-posn struct)
             (game-a-posn struct)))
(check-expect (change-dir (make-posn 2 3) (make-game 0 (make-posn 1 2) empty (make-posn 2 3)))
              (make-game 0 (make-posn 2 3) empty (make-posn 2 3)))

(define (change-p-posn list struct)
  (make-game (game-score struct)
             (game-dir struct)
             list
             (game-a-posn struct)))
(check-expect (change-p-posn (list (make-posn 2 3)) (make-game 0 (make-posn 1 2) empty (make-posn 2 3)))
              (make-game 0 (make-posn 1 2) (list (make-posn 2 3)) (make-posn 2 3)))

(define (change-a-posn posn struct)
  (make-game (game-score struct)
             (game-dir struct)
             (game-p-posn struct)
             posn))
(check-expect (change-a-posn (make-posn 2 4) (make-game 0 (make-posn 1 2) empty (make-posn 2 3)))
              (make-game 0 (make-posn 1 2) empty (make-posn 2 4)))

;image designs
(define background (square 1000 "solid" "black"))
(define snake-body (square 50 "solid" "white"))
(define apple (square 50 "solid" "red"))

;drawing images
(define (draw-apple struct)
  (place-image apple
               (posn-x (game-a-posn struct))
               (posn-y (game-a-posn struct))
               background))
(define (draw-snake-body list back)
  (cond [(not (empty? list))
         (place-image snake-body
                      (posn-x (first list))
                      (posn-y (first list))
                      (draw-snake-body (rest list) back))]
        [else back]))
(define (draw-handler struct)
  (draw-snake-body (game-p-posn struct) (draw-apple struct)))
                             
;key-handler: struct key->struct
(define (key-changer struct key)
  (cond
    [(key=? key "up") (change-dir (make-posn 0 -50) struct)]
    [(key=? key "down") (change-dir (make-posn 0 50) struct)]
    [(key=? key "right") (change-dir (make-posn 50 0) struct)]
    [(key=? key "left") (change-dir (make-posn -50 0) struct)]
    [else struct]))
(define (key-handler struct key)
  (cond [(and (= (abs (posn-x (game-dir (key-changer struct key))))
                 (abs (posn-x (game-dir struct))))
              (= (abs (posn-y (game-dir (key-changer struct key))))
                 (abs (posn-y (game-dir struct)))))
         struct]
        [else (change-dir (game-dir (key-changer struct key)) struct)]))
(check-expect (key-handler (make-game 0 (make-posn 0 50) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)) "up")
              (make-game 0 (make-posn 0 50) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)))
(check-expect (key-handler (make-game 0 (make-posn 50 0) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)) "up")
              (make-game 0 (make-posn 0 -50) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)))

;snake-body functions

;move:struct->struct
;moves the snake in the direction
(define (move struct)
  (change-p-posn (append (list (make-posn (+ (posn-x (first (game-p-posn struct))) (posn-x (game-dir struct)))
                                          (+ (posn-y (first (game-p-posn struct))) (posn-y (game-dir struct)))))
                         (game-p-posn struct))
                 struct))
(check-expect (move (make-game 0 (make-posn 0 1) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)))
              (make-game 0 (make-posn 0 1) (list (make-posn 1 2) (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)))

;restrict-length: struct->struct
;restricts the length of the p-posn list to the score
(define (restrict-length struct)
  (cond [(> (length (game-p-posn struct)) (game-score struct))
         (restrict-length (change-p-posn (reverse (rest (reverse (game-p-posn struct)))) struct))]
        [else struct]))
(check-expect (restrict-length (make-game 1 (make-posn 0 1) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 0 0)))
              (make-game 1 (make-posn 0 1) (list (make-posn 1 1)) (make-posn 0 0)))

;at-apple?:struct->boolean
;checks whether snake head is at apple
(define (at-apple? struct)
  (cond [(equal? (game-a-posn struct)
                 (first (game-p-posn struct)))
         true]
        [else false]))
(check-expect (at-apple? (make-game 1 (make-posn 0 1) (list (make-posn 1 1) (make-posn 2 1)) (make-posn 1 1)))
              true)

;generate-apple:struct->strcut
;generated new position of the apple
(define (generate-apple struct)
  (change-a-posn (make-posn (+ 25 (* (random 20) 50))
                            (+ 25 (* (random 20) 50)))
                 struct))

;game ending conditions

;out-of-bounds?: struct->boolean
(define (out-of-bounds? struct)
  (cond [(or (or (> (posn-x (first (game-p-posn struct))) 1000)
                 (< (posn-x (first (game-p-posn struct))) 0))
             (or (> (posn-y (first (game-p-posn struct))) 1000)
                 (< (posn-y (first (game-p-posn struct))) 0)))
         true]
        [else false]))

;hit-itself?: struct->boolean
(define (hit-itself? struct)
  (cond [(> (length (game-p-posn struct)) 2) 
         (member (first (game-p-posn struct))
                 (rest (rest (game-p-posn struct))))]
        [else false]))

;end-game?: struct->boolean
(define (end-game? struct)
  (or (hit-itself? struct)
      (out-of-bounds? struct)))

(define (end-screen struct)
  (place-image (text (number->string (game-score struct)) 100 "white")
               500 500 background))

;tick-handler:struct->struct
(define (tick-handler struct)
  (cond [(at-apple? struct)
         (generate-apple (restrict-length (move (change-score (+ 1 (game-score struct)) struct))))]
        [else (restrict-length (move struct))]))

(big-bang (make-game 1 (make-posn 0 0) (list (make-posn 25 25)) (make-posn 975 975))
  (on-tick tick-handler 0.25)
  (on-key key-handler)
  (on-draw draw-handler)
  (stop-when end-game? end-screen))





                                 
                                 
                               
