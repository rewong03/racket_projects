;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname go) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)
(provide (all-defined-out))

;p: x y c (color)
(define-struct p (x y c))

(define (change-x p x)
  (make-p x
          (p-y p)
          (p-c p)))
(define (change-y p y)
  (make-p (p-x p)
          y
          (p-c p)))
(define (change-c p c)
  (make-p (p-x p)
          (p-y p)
          c))

;game-l: l1 (list of pieces on the board) l2 (list of captured)
;(define-struct game-1 (l1 l2))

(define TILE_SIZE 50)
(define BOARD_SIZE 8)

(define (make-row n s)
  (cond [(= n 1)
         (frame (rectangle s s "solid" "beige"))]
        [else (beside (frame (rectangle s s "solid" "beige"))
                      (make-row (- n 1) s))]))

(define (make-board-h x y)
  (cond [(= y 1)
         (make-row x TILE_SIZE)]
        [else (above (make-row x TILE_SIZE)
                     (make-board-h x (- y 1)))]))

(define (make-board n)
  (overlay (make-board-h n n)
           (rectangle (* TILE_SIZE (+ 2 n))
                      (* TILE_SIZE (+ 2 n))
                      "solid" "beige")))
;switch-piece-color:p (piece) ->piece
(define (switch-piece-color p)
  (if (equal? (p-c p) "b")
      (change-c p "w")
      (change-c p "b")))

;turns piece coordinates into human coordinates
(define (coord->xy x y)
  (make-posn (* (+ 1 x) TILE_SIZE)
             (* (+ 1 y) TILE_SIZE)))

;turns human coordinates into piece coordinates
(define (xy->coord x y)
  (make-posn (- (ceiling (/ (- x 25) TILE_SIZE)) 1)
             (- (ceiling (/ (- y 25) TILE_SIZE)) 1)))

;make-stone: struct (piece)->image
(define (make-stone struct)
  (if (equal? (p-c struct) "w")
      (overlay (circle (/ TILE_SIZE 2) "outline" "black")
               (circle (/ TILE_SIZE 2) "solid" "white"))
      (circle (/ TILE_SIZE 2) "solid" "black")))

(define (draw-h model)
  (cond [(empty? model)
         (make-board BOARD_SIZE)]
        [(empty? (rest model))
         (place-image (make-stone (first model))
                      (posn-x (coord->xy (p-x (first model)) (p-y (first model))))
                      (posn-y (coord->xy (p-x (first model)) (p-y (first model))))
                      (make-board BOARD_SIZE))]
        [else (place-image (make-stone (first model))
                           (posn-x (coord->xy (p-x (first model)) (p-y (first model))))
                           (posn-y (coord->xy (p-x (first model)) (p-y (first model))))
                           (draw-h (rest model)))]))

;piece-in-list?: struct (piece) l (list)->boolean
(define (piece-in-list? struct l)
  (cond [(empty? l)
         false] 
        [(equal? struct (first l))
         true]
        [else (piece-in-list? struct (rest l))]))
(check-expect (piece-in-list? (make-p 1 3 "b") (list (make-p 1 1 "w") (make-p 2 3 "b") (make-p 1 3 "b")))
              true)
(check-expect (piece-in-list? (make-p 5 3 "b") (list (make-p 1 1 "w") (make-p 2 3 "b") (make-p 1 3 "b")))
              false)


;posn-in-list?: p (posn) l (list)->boolean
(define (posn-in-list? p l)
  (cond [(empty? l)
         false]
        [(equal? p (make-posn (p-x (first l))
                              (p-y (first l))))
         true]
        [else (posn-in-list? p (rest l))]))
(check-expect (posn-in-list? (make-posn 1 3) (list (make-p 1 1 "w") (make-p 2 3 "b") (make-p 1 3 "b")))
              true)

;posn-from-list: p (posn) l (list)->struct (piece)
(define (posn-from-list p l)
  (cond [(empty? l)
         false]
        [(equal? p (make-posn (p-x (first l))
                              (p-y (first l))))
         (first l)]
        [else (posn-from-list p (rest l))]))
(check-expect (posn-from-list (make-posn 1 3) (list (make-p 1 1 "w") (make-p 2 3 "b") (make-p 1 3 "b")))
              (make-p 1 3 "b"))

;find-color: p (posn) l (list)-> string (color)
(define (find-color p l)
  (if (posn-in-list? p l)
      (p-c (posn-from-list p l))
      false))
(check-expect (find-color (make-posn 1 3) (list (make-p 1 1 "w") (make-p 2 3 "b") (make-p 1 3 "b")))
              "b")

;add-posn-help: p1 p2-> p
(define (add-posn-help p1 p2)
  (make-posn (+ (posn-x p1)
                (posn-x p2))
             (+ (posn-y p1)
                (posn-y p2))))

(define (add-posn struct p)
  (make-p (posn-x (add-posn-help (make-posn (p-x struct)
                                            (p-y struct))
                                 p))
          (posn-y (add-posn-help (make-posn (p-x struct)
                                            (p-y struct))
                                 p))
          (p-c struct)))

(define (add-posn-struct struct p)
  (make-posn (p-x (add-posn struct p))
             (p-y (add-posn struct p))))

(define (add-two-posns p1 p2)
  (make-posn (+ (posn-x p1)
                (posn-x p2))
             (+ (posn-y p1)
                (posn-y p2))))

;adjacent-same-color?: p (piece) d (direction, posn) 1 (list)->boolean
;checks whether the piece adjacent to p in direction d is the same color (and if it exists)
(define (adjacent-same-color? p d l)
  (if (piece-in-list? (add-posn p d) l)
      true false))
(check-expect (adjacent-same-color? (make-p 1 1 "b") (make-posn 0 1) (list (make-p 1 1 "b") (make-p 2 5 "b") (make-p 1 2 "b")))
              true)

;find-adjacent-help1: p (piece) d (direction) l (list)->list (of pieces)
;makes list of adjacent pieces of same color
(define (find-adjacent-help1 p d l)
  (cond [(adjacent-same-color? p d l)
         (posn-from-list (add-posn-struct p d) l)]
        [else false]))
(check-expect (find-adjacent-help1 (make-p 1 1 "b") (make-posn 0 1) (list (make-p 1 1 "b") (make-p 2 5 "b") (make-p 1 2 "b")))
              (make-p 1 2 "b"))

(define x-list (list 0 0 -1 1))
(define y-list (list -1 1 0 0))

;find-adjacent-help2: p (posn) x1 (list for x values) y1 (list for y values) l (list of pieces)
;finds adjacent pieces in the directions defined by x1 and y1
(define (find-adjacent-help2 p xl yl l)
  (cond [(empty? yl)
         (list p)]
        [(adjacent-same-color? p (make-posn (first xl) (first yl)) l)
         (list* (find-adjacent-help1 p (make-posn (first xl) (first yl)) l)
                (find-adjacent-help2 p (rest xl) (rest yl) l))]
        [else (find-adjacent-help2 p (rest xl) (rest yl) l)]))

;find-adjacent: p (piece) l (list)->list (of pieces)
;finds all adjacent pieces to p that are the same color
(define (find-adjacent p l)
  (find-adjacent-help2 p x-list y-list l))
(check-expect (find-adjacent (make-p 1 2 "b") (list (make-p 3 4 "w") (make-p 1 1 "b") (make-p 2 2 "b")))
              (list (make-p 1 1 "b") (make-p 2 2 "b") (make-p 1 2 "b")))
(check-expect (find-adjacent (make-p 1 2 "b") (list (make-p 3 4 "w") (make-p 1 1 "w") (make-p 2 2 "w")))
              (list (make-p 1 2 "b")))

;remove-from-list: p (piece) l (list)->list
;removes the piece from the list
(define (remove-from-list p l)
  (cond [(empty? l)
         empty]
        [(equal? p (first l))
         (remove-from-list p (rest l))]
        [else (list* (first l)
                     (remove-from-list p (rest l)))]))
(check-expect (remove-from-list (make-p 1 2 "w") (list (make-p 2 5 "b") (make-p 1 2 "w") (make-p 1 2 "w") (make-p 3 4 "b")))
              (list (make-p 2 5 "b") (make-p 3 4 "b")))

;remove-list-from-list: l1 (list of pieces you want removed) l2 (list of all pieces)-> list
(define (remove-list-from-list l1 l2)
  (cond [(empty? l1)
         l2]
        [else (remove-list-from-list (rest l1)
                                     (remove-from-list (first l1) l2))]))

;remove-extras: l (list)->list
;removes same values in a list
(define (remove-extras l)
  (cond [(empty? l)
         empty]
        [else (list* (first l)
                     (remove-extras (remove-from-list (first l) (rest l))))]))
(check-expect (remove-extras (list (make-p 1 1 "b") (make-p 2 1 "w") (make-p 1 1 "b") (make-p 2 1 "w")))
              (list (make-p 1 1 "b") (make-p 2 1 "w")))

;list-in-list?: l1 (list of pieces) l2 (list of pieces)->boolean
;checks whether all elements in l1 are in l2
(define (list-in-list? l1 l2)
  (cond [(empty? l1)
         true]
        [(not (piece-in-list? (first l1) l2))
         false]
        [else (list-in-list? (rest l1) l2)]))
(check-expect (list-in-list? (list (make-p 1 2 "b") (make-p 2 1 "b"))
                             (list (make-p 3 5 "b") (make-p 1 2 "b") (make-p 2 1 "b")))
              true)
(check-expect (list-in-list? (list (make-p 10 2 "b") (make-p 20 1 "b"))
                             (list (make-p 3 5 "b") (make-p 1 2 "b") (make-p 2 1 "b")))
              false)

;find-adjacent-list: l1 (of pieces you want to find adjacents) l2 (list of all pieces)->list of pieces
(define (find-adjacent-list l1 l2)
  (cond [(empty? l1)
         empty]
        [else (remove-extras (append (find-adjacent (first l1) l2)
                                     (find-adjacent-list (rest l1) l2)))]))

;all-adjacents-found-help?: l1 l2 l3->boolean
;sees if all the pieces have their adjacents in l1
;l1 is the same as l3
(define (all-adjacents-found-help? l1 l2 l3)
  (cond [(empty? l1)
         true]
        [(not (list-in-list? (find-adjacent (first l1) l2) l3))
         false]
        [else (all-adjacents-found-help? (rest l1) l2 l3)]))

(check-expect (all-adjacents-found-help? (list (make-p 1 2 "b") (make-p 1 3 "b") (make-p 1 4 "b"))
                                         (list (make-p 1 2 "b") (make-p 1 3 "b") (make-p 1 4 "b") (make-p 1 5 "w"))
                                         (list (make-p 1 2 "b") (make-p 1 3 "b") (make-p 1 4 "b")))
              true)

;all-adjacents-found?: l1 l2->boolean
(define (all-adjacents-found? l1 l2)
  (all-adjacents-found-help? l1 l2 l1))

;adjacent-found-unfound-help: l1 l2 l3 l4-> 2 lists
(define (adjacent-found-unfound-help l1 l2 l3 l4)
  (cond [(empty? l3)
         (list l1 l2)]
        [(list-in-list? (find-adjacent (first l3) l4) (append l1 l2 l3))
         (adjacent-found-unfound-help l1 (list* (first l3)
                                                l2) (rest l3) l4)]
        [else (adjacent-found-unfound-help (list* (first l3)
                                                  l1) l2 (rest l3) l4)]))
(define (adjacent-found-unfound l1 l2)
  (adjacent-found-unfound-help empty empty l1 l2))
(check-expect (adjacent-found-unfound (list (make-p 1 1 "b")) (list (make-p 1 1 "b")))
              (list empty (list (make-p 1 1 "b"))))
(check-expect (adjacent-found-unfound (list (make-p 1 2 "b") (make-p 1 3 "b") (make-p 1 4 "b") (make-p 1 5 "b"))
                                      (list (make-p 1 1 "b") (make-p 1 2 "b") (make-p 1 3 "b") (make-p 2 3 "w") (make-p 1 4 "b")
                                            (make-p 1 5 "b") (make-p 1 6 "b")))
              (list (list (make-p 1 5 "b") (make-p 1 2 "b"))
                    (list (make-p 1 4 "b") (make-p 1 3 "b"))))

;find-chain: l1 (list of pieces) l2 (list of pieces)->list (of adjacent pieces)
(define (find-chain-help l1 l2 l3)
  (cond [(empty? l1)
         l3]
        [(not (list-in-list? (find-adjacent (first l1) l2) (append l1 l3)))
         (find-chain-help (append l1 (rest (reverse (find-adjacent (first l1) l2))))
                          l2 (list* (first l1) l3))]
        [else (find-chain-help (rest l1) l2 (list* (first l1) l3))]))
(define (find-chain l1 l2)
  (remove-extras (find-chain-help l1 l2 empty)))

(check-expect (find-chain (list (make-p 1 1 "b"))
                          (list (make-p 1 1 "b") (make-p 1 2 "b") (make-p 1 3 "b") (make-p 2 3 "w") (make-p 1 4 "b")
                                (make-p 1 5 "b") (make-p 1 6 "b")))
              (list (make-p 1 4 "b") (make-p 1 6 "b") (make-p 1 3 "b") (make-p 1 5 "b") (make-p 1 2 "b") (make-p 1 1 "b")))
(check-expect (find-chain (list (make-p 6 4 "b"))
                          (list (make-p 6 3 "b") (make-p 6 2 "w") (make-p 6 4 "b") (make-p 5 4 "b") (make-p 5 3 "b")))
              (list (make-p 5 3 "b") (make-p 6 4 "b") (make-p 6 3 "b") (make-p 5 4 "b")))
(check-expect (find-chain (list (make-p 4 4 "b"))
                          (list (make-p 4 5 "b") (make-p 4 6 "b") (make-p 5 6 "b") (make-p 6 6 "b") (make-p 6 5 "b")
                                (make-p 6 4 "b") (make-p 5 4 "b") (make-p 4 4 "b")))
              (list (make-p 6 4 "b") (make-p 6 6 "b") (make-p 4 5 "b") (make-p 5 6 "b") (make-p 6 5 "b")
                    (make-p 5 4 "b") (make-p 4 4 "b") (make-p 4 6 "b")))

;completely-surrounded?: p (piece that you're checking) l (list of all pieces)-> boolean
;checks whether all the liberties of a piece are occupied
;if the piece is part of a chain, it will check all pieces of the chain
(define (completely-surrounded-help? l1 l2)
  (cond [(empty? l1)
         true]
        [(and (or (posn-in-list? (add-posn-struct (first l1) (make-posn 0 1)) l2)
                  (not (posn-in-bounds? (add-two-posns (posn-from-piece (first l1)) (make-posn 0 1)))))              
              (or (posn-in-list? (add-posn-struct (first l1) (make-posn 0 -1)) l2)
                  (not (posn-in-bounds? (add-two-posns (posn-from-piece (first l1)) (make-posn 0 -1)))))
              (or (posn-in-list? (add-posn-struct (first l1) (make-posn 1 0)) l2)
                  (not (posn-in-bounds? (add-two-posns (posn-from-piece (first l1)) (make-posn 1 0)))))
              (or (posn-in-list? (add-posn-struct (first l1) (make-posn -1 0)) l2)
                  (not (posn-in-bounds? (add-two-posns (posn-from-piece (first l1)) (make-posn -1 0))))))
         (completely-surrounded-help? (rest l1) l2)]
        [else false]))

(define (completely-surrounded? p l)
  (completely-surrounded-help? (find-chain (list p) l) l))
(check-expect (completely-surrounded? (make-p 5 4 "w")
                                      (list (make-p 6 4 "b") (make-p 5 5 "b") (make-p 4 4 "b") (make-p 5 3 "b") (make-p 5 4 "w")))
              true)

;remove-captured: p (piece) l (list)-> list
(define (remove-captured p l)
  (cond [(completely-surrounded? p l)
         (remove-list-from-list (find-chain p l) l)]
        [else l]))

(define (remove-captured-chain l1 l2)
  (cond [(empty? l1)
         l2]
        [else (remove-captured-chain (rest l1)
                                     (remove-from-list (first l1) l2))]))

;find-adjacent-chain: p (piece) l (list)->list
;finds the chains of pieces next to p that are the opposite color
(define (find-adjacent-chain p l)
  (find-chain (reverse (rest (reverse (find-adjacent (switch-piece-color p) l)))) l))

;results-in-capture?: p l->boolean
;checks whether your move results in the capture of opponent's chain
(define (results-in-capture? p l)
  (cond [(empty? l)
         false]
        [else (completely-surrounded-help? (find-adjacent-chain p l) (list* p l))]))

;update-after-remove: model x y-> model
;removes captured chains and updates the model with new piece
(define (update-after-remove model x y)
  (if (equal? (p-c (first model)) "w")
      (remove-captured-chain (find-adjacent-chain (make-p (posn-x (xy->coord x y))
                                                          (posn-y (xy->coord x y))
                                                          (p-c (switch-piece-color (first model))))
                                                  model)
                             (list* (make-p (posn-x (xy->coord x y))
                                            (posn-y (xy->coord x y))
                                            "b")
                                    model))
      (remove-captured-chain (find-adjacent-chain (make-p (posn-x (xy->coord x y))
                                                          (posn-y (xy->coord x y))
                                                          (p-c (switch-piece-color (first model))))
                                                  model) (list* (make-p (posn-x (xy->coord x y))
                                                                        (posn-y (xy->coord x y))
                                                                        "w")
                                                                model))))

;suicide-rule: model x y event->boolean
(define (suicide-rule model x y event)
  (if (empty? model)
      false (and (mouse=? event "button-down")
                 (completely-surrounded? (make-p (posn-x (xy->coord x y))
                                                 (posn-y (xy->coord x y))
                                                 (p-c (switch-piece-color (first model))))
                                         model))))
              
(define (mouse-h model x y event)
  (cond [(suicide-rule model x y event)
         model]
        [(and (mouse=? event "button-down")
              (equal? (p-c (first model)) "w")
              (results-in-capture? (make-p (posn-x (xy->coord x y))
                                           (posn-y (xy->coord x y))
                                           (p-c (switch-piece-color (first model))))
                                   model))
         (update-after-remove model x y)]
        [(and (mouse=? event "button-down")
              (equal? (p-c (first model)) "b")
              (results-in-capture? (make-p (posn-x (xy->coord x y))
                                           (posn-y (xy->coord x y))
                                           (p-c (switch-piece-color (first model))))
                                   model))
         (update-after-remove model x y)]
        [(and (mouse=? event "button-down")
              (equal? (p-c (first model)) "w"))
         (list* (make-p (posn-x (xy->coord x y))
                        (posn-y (xy->coord x y))
                        "b")
                model)]
        [(and (mouse=? event "button-down")
              (equal? (p-c (first model)) "b"))
         (list* (make-p (posn-x (xy->coord x y))
                        (posn-y (xy->coord x y))
                        "w")
                model)]
        [else model]))

;find score by checking adjacent posns to see whether it is a piece using
;posn-in-list?. add the piece to a list, the checked posn into another list, and the unchecked posns
;into a third list. continue to find adjacent posns until all of them have been searched.
;if the posns in the list of pieces belong to one color, add points to that color, otherwise, neither colors get points

;posn-in-bounds?: posn->boolean
;checks whether posn is on the board
(define (posn-in-bounds? p)
  (and (and (<= 0 (posn-x p))
            (<= 0 (posn-y p)))
       (and (>= BOARD_SIZE (posn-x p))
            (>= BOARD_SIZE (posn-y p)))))
(check-expect (posn-in-bounds? (make-posn -1 5))
              false)
(check-expect (posn-in-bounds? (make-posn 10 5))
              false)

;keep-posn-in-bounds: list (of posns)->list
;removes posns from list that aren't on the board
(define (keep-posn-in-bounds l)
  (cond [(empty? l)
         empty]
        [(posn-in-bounds? (first l))
         (list* (first l)
                (keep-posn-in-bounds (rest l)))]
        [else (keep-posn-in-bounds (rest l))]))
(check-expect (keep-posn-in-bounds (list (make-posn 1 1) (make-posn 10 10) (make-posn 5 5)))
              (list (make-posn 1 1) (make-posn 5 5)))                

;find-adjacent-posn-help: direction (posn) posn->posn
(define (find-adjacent-posn-help d p)
  (add-two-posns d p))

;find-all-adjacent-posn-help: x (x of direction) y (y of direction) posn l (empty list)->list
(define (find-all-adjacent-help x y p l)
  (cond [(empty? x)
         l]
        [(posn-in-bounds? (add-two-posns (make-posn (first x) (first y)) p))
         (find-all-adjacent-help (rest x) (rest y) p (list* (add-two-posns (make-posn (first x) (first y)) p)
                                                            l))]
        [else (find-all-adjacent-help (rest x) (rest y) p l)]))

;find-all-adjacent-posns: posn->posn
(define (find-all-adjacent-posns p)
  (find-all-adjacent-help x-list y-list p empty))

;seperate-adjacent-piece-posns-help: p (piece) l1 (list of pieces) l2 (empty list) l3 (empty list)
;l4 (list of pieces) -> l2 (list of pieces) l3 (list of posns)
(define (seperate-adjacent-piece-posns-help p l1 l2 l3 l4)
  (cond [(empty? l1)
         (list l2 l3)]
        [(posn-in-list? (first l1) l4)
         (seperate-adjacent-piece-posns-help p (rest l1)
                                             (list* (posn-from-list (first l1) l4) l2)
                                             l3 l4)]
        [else (seperate-adjacent-piece-posns-help p (rest l1)
                                                  l2
                                                  (list* (first l1) l3) l4)]))

;seperate-adjacent-piece-posns: posn->l1 (pieces) l2 (posns)
(define (seperate-adjacent-piece-posns p l)
  (seperate-adjacent-piece-posns-help p (find-all-adjacent-posns p)
                                      empty empty l))

;posn-from-piece: piece->posn
(define (posn-from-piece p)
  (make-posn (p-x p)
             (p-y p)))

;list-posn-from-piece: list of pieces-> list of posns
(define (list-posn-from-piece l)
  (cond [(empty? l)
         empty]
        [else (list* (posn-from-piece (first l))
                     (list-posn-from-piece (rest l)))]))

;list-piece-from-posn: l1 (list of posns) l2 (list of pieces) ->list of pieces
(define (list-piece-from-posn l1 l2)
  (cond [(empty? l1)
         empty]
        [(posn-in-list? (first l1) l2)
         (list* (posn-from-list (first l1) l2)
                (list-piece-from-posn (rest l1) l2))]
        [else (list-piece-from-posn (rest l1) l2)]))

;posn-list-in-list?: l1 (list of posns) l2->boolean
(define (posn-list-in-list? l1 l2)
  (cond [(empty? l1)
         true]
        [(member? (first l1) l2)
         (posn-list-in-list? (rest l1) l2)]
        [else false]))

;seperate-piece-and-posn: l1 (list of posns) l2 (list of pieces)->2 lists (list of posn) (list of posns that are pieces)
(define (seperate-piece-and-posn-help l1 l2 l3 l4)
  (cond [(empty? l1)
         (list l3 l4)]
        [(posn-in-list? (first l1) l2)
         (seperate-piece-and-posn-help (rest l1) l2 l3 (list* (first l1) l4))]
        [else (seperate-piece-and-posn-help (rest l1) l2 (list* (first l1) l3) l4)]))
(define (seperate-piece-and-posn l1 l2)
  (seperate-piece-and-posn-help l1 l2 empty empty))

;seperate-adjacents: l1 (list of pieces) l2 (list of pieces) l3 (empty) l4 (empty) l5 (empty)->
;l3 (list of found adjacents) l4 (list of unfound adjacents) l5 (list of adjacents that are pieces)
(define (seperate-found-unfound-adjacents-help l1 l2 l3)
  (cond [(empty? l1)
         (list l2 l3)]
        [(posn-list-in-list? (find-all-adjacent-posns (first l1)) (append l1 l2 l3))
         (seperate-found-unfound-adjacents-help (rest l1) (list* (first l1) l2) l3)]
        [else (seperate-found-unfound-adjacents-help (rest l1) l2 (list* (first l1) l3))]))

(define (seperate-found-unfound-adjacents l)
  (seperate-found-unfound-adjacents-help l empty empty))


;find-territory-help: l1 (list of posn in territory) l2 (list of pieces) l3 (empty) l4 (empty)->
;l3 (list of posns) l4 (list of posns that are pieces)
(define (find-territory-help l1 l2 l3 l4)
  (cond [(empty? l1)
         (list l3 l4)]
        [(posn-in-list? (first l1) l2)
         (find-territory-help (rest l1) l2
                              l3 (list* (first l1) l4))]
        [(not (posn-list-in-list? (find-all-adjacent-posns (first l1)) (append l1 l3 l4)))
         (find-territory-help (remove-extras (append (rest l1) (find-all-adjacent-posns (first l1)))) l2
                              (append (first (seperate-piece-and-posn (list (first l1)) l2)) l3)
                              (append (rest (seperate-piece-and-posn (list (first l1)) l2)) l4))]
        [else (find-territory-help (rest l1) l2
                                   (append (first (seperate-piece-and-posn (list (first l1)) l2)) l3)
                                   (append (rest (seperate-piece-and-posn (list (first l1)) l2)) l4))]))

(define (find-territory l1 l2)
  (list (remove-extras (first (find-territory-help l1 l2 empty empty)))
        (remove-extras (first (rest (find-territory-help l1 l2 empty empty))))))

;colors-different? list (of pieces)->boolean
(define (colors-different-help? p l)
  (cond [(empty? l)
         true]
        [(equal? (p-c p) (p-c (first l)))
         (colors-different-help? p (rest l))]
        [else false]))
(define (colors-different? l)
  (colors-different-help? (first l) l))

;is-valid-points? p (posn in territory) l (list of pieces)->boolean
(define (is-valid-points? p l)
  (if (equal? 1 (length (first (rest (find-territory (list p) l)))))
      #f (colors-different? (list-piece-from-posn (first (rest (find-territory (list p) l))) l))))

(define (whose-points? p l)
  (cond [(not (is-valid-points? p l))
         #f]
        [else (p-c (first (list-piece-from-posn (first (rest (find-territory (list p) l))) l)))]))

(define (list-of-all-posns-help x1 y1 x2 y2)
  (cond [(equal? y1 y2)
         empty]
        [(equal? x1 x2)
         (list-of-all-posns-help 0 (+ 1 y1) x2 y2)]
        [else (list* (make-posn x1 y1)
                     (list-of-all-posns-help (+ 1 x1) y1 x2 y2))]))

(define (list-of-all-posns num)
  (list-of-all-posns-help 0 0 (+ 1 num) (+ 1 num)))





