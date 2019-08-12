;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname othello) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)

(define WIDTH 40)
(define HEIGHT 40)

(define tile (frame (rectangle WIDTH HEIGHT "solid" "green")))
(define (row-maker n)
  (cond [(= n 1)
         tile]
        [else (beside tile
                      (row-maker (- n 1)))]))
(define (board-maker x y)
  (cond [(= y 1)
         (row-maker x)]
        [else (above (row-maker x)
                     (board-maker x (- y 1)))]))
;board
(define board
  (board-maker 8 8))
;black-piece
(define black-piece
  (circle 20 "solid" "black"))
;white-piece
(define white-piece
  (circle 20 "solid" "white"))
;p: x (num) y (num) c ("b" or "w")
(define-struct p (x y c))

(define-struct game (l t))

(define start-game
  (make-game (list (make-p 4 4 "b")
                   (make-p 4 5 "w")
                   (make-p 5 4 "w")
                   (make-p 5 5 "b")) 0))

(define (b-or-w? piece)
  (if (equal? (p-c piece) "b")
      black-piece
      white-piece))

(define (opposite-color piece)
  (if (equal? (p-c piece) "b")
      "w" "b"))

(define (coord->xy piece)
  (make-posn (floor (- (* (p-x piece) WIDTH) (* 0.5 WIDTH)))
             (floor (- (* (p-y piece) HEIGHT) (* 0.5 HEIGHT)))))

;draw-h: list (of pieces)-> img
(define (draw-help model)
  (cond [(empty? (rest model))
         (place-image (b-or-w? (first model))
                      (posn-x (coord->xy (first model)))
                      (posn-y (coord->xy (first model)))
                      board)]
        [else (place-image (b-or-w? (first model))
                           (posn-x (coord->xy (first model)))
                           (posn-y (coord->xy (first model)))
                           (draw-help (rest model)))]))

;piece-from-list: x (posn) data (list of pieces)-> piece (struct)
(define (piece-from-list x data)
  (cond [(and (empty? (rest data))
              (equal? x (make-posn (p-x (first data))
                                   (p-y (first data)))))
         (first data)]
        [(empty? (rest data))
         #f]
        [(equal? x (make-posn (p-x (first data))
                              (p-y (first data))))
         (first data)]
        [else (piece-from-list x (rest data))]))

;(check-expect (piece-from-list (make-posn 1 1) start-game)
;             #f)
;(check-expect (piece-from-list (make-posn 4 4) start-game)
;             (make-p 4 4 "b"))

;piece-in-list?: x (posn) data (list of pieces)-> boolean
(define (piece-in-list? x data)
  (cond [(equal? (piece-from-list x data) #f)
         #f]
        [else #t]))

;(check-expect (piece-in-list? (make-posn 1 1) start-game)
;            #f)

;a legal play in othello is determined by whether the move results in an opponents piece being overturned
;the following helper functions check whether there are opposing pieces after the most recent play
;and a player's piece at the end of the opposing pieces

;adjacent-valid?: x (num) y (num) data (list of pieces)-> boolean
;checks whether the adjacent piece is the opposite color
;x and y determine the direction that you are checking
;it works similarly to a slope
;ex. (adjacent-valid? 0 1 data) would check going up
(define (adjacent-valid? x y data)
  (cond [(not (piece-in-list? (make-posn (- (p-x (first data)) x)
                                         (- (p-y (first data)) y))
                              data))
         #f]
        [(equal? (piece-from-list (make-posn (- (p-x (first data)) x)
                                             (- (p-y (first data)) y))
                                  data)
                 (make-p (- (p-x (first data)) x)
                         (- (p-y (first data)) y)
                         (opposite-color (first data))))
         #t]
        [else #f]))

(check-expect (adjacent-valid? 0 1 (list (make-p 2 2 "b")
                                         (make-p 4 3 "w")
                                         (make-p 2 1 "w")))
              #t)

(define (valid-1d? x y x1 y1 data)
  (cond [(not (piece-in-list? (make-posn (- (p-x (first data)) x)
                                         (- (p-y (first data)) y))
                              data))
         #f]
        [(not (adjacent-valid? x1 y1 data))
         #f]
        [(and (adjacent-valid? x1 y1 data)
              (equal? (opposite-color (first data)) (p-c (piece-from-list (make-posn (- (p-x (first data)) x)
                                                                                     (- (p-y (first data)) y))
                                                                          data))))
         (valid-1d? (+ x x1) (+ y y1) x1 y1 data)]
        [else #t]))

(define (valid-h? x y x1 y1 data)
  (cond [(equal? (opposite-color (first data)) (p-c (piece-from-list (make-posn (- (p-x (first data)) x)
                                                                                (- (p-y (first data)) y))
                                                                     data)))
         (valid-h? (+ x1 x) (+ y1 y) x1 y1  (list* (first data)
                                                   (change-color (make-p (- (p-x (first data)) x)
                                                                         (- (p-y (first data)) y)
                                                                         (opposite-color (first data)))
                                                                 data)
                                                   (remove-from-list
                                                    (make-p (- (p-x (first data)) x)
                                                            (- (p-y (first data)) y)
                                                            (opposite-color (first data)))
                                                    (rest data))))]
        [else data]))

;change-color: x (piece) data (list of pieces)-> piece
(define (change-color x data)
  (cond [(empty? (rest data))
         (make-p (p-x x)
                 (p-y x)
                 (opposite-color x))]
        [(equal? x (first data))
         (make-p (p-x x)
                 (p-y x)
                 (opposite-color x))]
        [else (change-color x (rest data))]))

;remove-from-list: x (piece) data (list of pieces)-> list
(define (remove-from-list x data)
  (cond [(empty? (rest data))
         (rest data)]
        [(equal? x (first data))
         (rest data)]
        [else (list* (first data)
                     (remove-from-list x (rest data)))]))

(define test (list (make-p 2 4 "b")
                   (make-p 4 3 "w")
                   (make-p 2 1 "w")
                   (make-p 2 3 "w")
                   (make-p 2 2 "w")))

(define (valid? data)
  (or (valid-1d? 0 1 0 1 data)
      (valid-1d? 1 0 1 0 data)
      (valid-1d? 1 1 1 1 data)
      (valid-1d? 0 -1 0 -1 data)
      (valid-1d? -1 0 -1 0 data)
      (valid-1d? -1 1 -1 1 data)
      (valid-1d? -1 -1 -1 -1 data)
      (valid-1d? 1 -1 1 -1 data)))

(define (change-line-h l1 l2 data)
  (cond [(empty? l1)
         data]
        [(valid-1d? (first l1) (first l2) (first l1) (first l2) data)
         (change-line-h (rest l1) (rest l2) (valid-h? (first l1) (first l2) (first l1) (first l2) data))]
        [else (change-line-h (rest l1) (rest l2) data)]))

(define (change-line data)
  (change-line-h (list 0 0 0 -1 -1 -1 1 1 1) (list -1 0 1 -1 0 1 -1 0 1) data))

(define (what-color-t model)
  (if (equal? "b" (game-t model))
      "b" "w"))

(define (opposite-color-t model)
  (if (equal? "b" (game-t model))
      "w" "b"))

(define (count-color c data)
  (if (equal? c "w")
      (count-w-h data 0)
      (count-b-h data 0)))

(define (mouse-h model x y event)
  (cond [(and (mouse=? event "button-down")
              (valid? (list* (make-p (ceiling (/ x WIDTH))
                                     (ceiling (/ y WIDTH))
                                     (what-color-t model))
                             (game-l model))))           
         (make-package model (list (ceiling (/ x WIDTH))
                                   (ceiling (/ y WIDTH))
                                   (game-t model)))]
        [else model]))

(define (receive-h model msg)
  (make-game (change-line (list* (make-p (first msg)
                                         (first (rest msg))
                                         (first (rest (rest msg))))
                                 (game-l model)))
             (game-t model)))

(define (count-w-h data num)
  (cond [(empty? data)
         num]
        [(equal? (p-c (first data)) "w")
         (count-w-h (rest data) (+ 1 num))]
        [else (count-w-h (rest data) num)]))

(define (count-b-h data num)
  (cond [(empty? data)
         num]
        [(equal? (p-c (first data)) "b")
         (count-b-h (rest data) (+ 1 num))]
        [else (count-b-h (rest data) num)]))

(define (draw-h model)
  (cond [(< (length (game-l model)) 64)
         (draw-help (game-l model))]
        [(and (< (length (game-l model)) 64)
              (< (count-b-h (game-l model) 0) (count-w-h (game-l model) 0))
              (equal? (game-t model) "w"))
         (overlay (text "You won!" 30 "black")
                  (empty-scene 320 320))]
        [(and (< (length (game-l model)) 64)
              (> (count-b-h (game-l model) 0) (count-w-h (game-l model) 0))
              (equal? (game-t model) "b"))
         (overlay (text "You won!" 30 "black")
                  (empty-scene 320 320))]
        [(and (< (length (game-l model)) 64)
              (< (count-b-h (game-l model) 0) (count-w-h (game-l model) 0))
              (not (equal? (game-t model) "w")))
         (overlay (text "You lost!" 30 "black")
                  (empty-scene 320 320))]
        [(and (< (length (game-l model)) 64)
              (> (count-b-h (game-l model) 0) (count-w-h (game-l model) 0))
              (not (equal? (game-t model) "b")))
         (overlay (text "You lost!" 30 "black")
                  (empty-scene 320 320))]))
         

(define (run-program n m)
  (big-bang m
            (on-draw draw-h)
            (on-mouse mouse-h)
            (on-receive receive-h)
            (register LOCALHOST) 
            (state #t)
            (close-on-stop #t)
            (name n)))

(launch-many-worlds (run-program "1" (make-game (game-l start-game)
                                                "b"))
                    (run-program "2" (make-game (game-l start-game)
                                                "w")))
                   
                    
                                     




                 
                                                             


