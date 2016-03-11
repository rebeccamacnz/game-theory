;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname MiniUltimatumGame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
#|-----------------------------------------DATA DEFINITIONS-----------------------------------------|#
;;A Player is a (make-player Number Number Number Boolean)
; - where the first Number is how much the player will demand if they are the proposer
; - the second Number is the lowest amount the player will accept if they are the responder
; - the third Number is the score of the player
; - and the Boolean indicates whether this player is proposing or responding (True = Proposer)
(define-struct player (propose respond score role))

;;A [Grid X] is a [List [List X]] where each inner list has the same length as the outer list

;;A World is a [Grid Player]

#|---------------------------------------------CONSTANTS--------------------------------------------|#
(define GRID-SIZE 20)
(define CELL-SIZE 10)
(define SCENE-SIZE (+ (* GRID-SIZE CELL-SIZE) 10))
(define COLORS '(Red Orange Yellow Green Blue Indigo Purple HotPink SaddleBrown Black))
(define RANDOM-WORLD1
  (list (list (make-player 3 7 0 false)
              (make-player 8 3 0 true)
              (make-player 9 6 0 true))
        (list (make-player 6 9 0 false)
              (make-player 9 9 0 false)
              (make-player 9 10 0 true))
        (list (make-player 9 3 0 true)
              (make-player 10 6 0 false)
              (make-player 3 1 0 true))))
#|---------------------------------------------FUNCTIONS--------------------------------------------|#

;;main : -> World
(define (main)
  (big-bang (initial-world)
            (on-tick tick-world .05)
            (to-draw draw-world)
            (stop-when when-to-stop)))

;;initial-world : -> World
;;Produces the initial world
(define (initial-world)
  (build-list GRID-SIZE
              (λ (x) (build-list GRID-SIZE
                                 (λ (y) (make-random-player))))))
(check-expect (length (initial-world)) GRID-SIZE)
(check-expect (andmap (λ (x) (= (length x) GRID-SIZE)) (initial-world)) true)
(check-expect (player? (list-ref (list-ref (initial-world) (random GRID-SIZE)) (random GRID-SIZE)))
              true)
(check-expect (andmap (λ (x) (andmap (λ (y) (zero? (player-score y))) x)) (initial-world)) true)

;;make-random-player : -> Player
;;Produces a random player with a score of 0
(define (make-random-player)
  (make-player (add1 (random 10))
               (add1 (random 10))
               0 (random-bool)))
(check-expect (player? (make-random-player)) true)
(check-expect (in-range? (player-propose (make-random-player)) 1 10) true)
(check-expect (in-range? (player-respond (make-random-player)) 1 10) true)
(check-expect (player-score (make-random-player)) 0)
(check-expect (boolean? (player-role (make-random-player))) true)

;;in-range? : Number Number Number -> Boolean
;;Determines if x is in the range [lo,hi]
(define (in-range? x lo hi)
  (and (>= x lo) (<= x hi)))
(check-expect (in-range? 2 1 10) true)
(check-expect (in-range? 1 1 3) true)
(check-expect (in-range? 4 2 4) true)
(check-expect (in-range? 5 2 4) false)
(check-expect (in-range? 0 1 10) false)

;;random-bool : -> Boolean
;;Produces true or false randomly
(define (random-bool)
  (zero? (random 2)))
(check-expect (boolean? (random-bool)) true)

;;tick-world : World -> World
;;Update the players' strategies as necessary
(define (tick-world w)
  (update-strategies (update-scores w)))
(check-expect
 (map (λ (row) (map player-score row))
      (tick-world RANDOM-WORLD1))
 '((1 0 0) (0 0 0) (0 1 1)))
(check-expect
 (map (λ (row) (map player-propose row))
      (tick-world RANDOM-WORLD1))
 (list (list 3 3 3) (list 6 9 3) (list 3 10 3)))
(check-expect
 (map (λ (row) (map player-respond row))
      (tick-world RANDOM-WORLD1))
 (list (list 7 3 6) (list 6 6 10) (list 3 6 1)))
(check-expect (tick-world '()) '())

;;update-scores : World -> World
;;Updates the scores of all the players based on how well they did against their neighbors
(define (update-scores w)
  (map (λ (rowi) (map (λ (coli) (update-player-score rowi coli w))
                      (build-list (length w) identity)))
       (build-list (length w) identity)))
(check-expect
 (map (λ (row) (map player-score row))
      (update-scores RANDOM-WORLD1))
 '((1 0 0) (0 0 0) (0 1 1)))
(check-expect (update-scores '()) '())

;;update-player-score : Nat Nat World -> Player
;;Updates the score of this player based on how well they did against their neighbors
(define (update-player-score rowi coli w)
  (local [(define neighbors (get-player-neighbors rowi coli w))
          (define me (list-ref (list-ref w rowi) coli))]
    (make-player (player-propose me) (player-respond me)
                 (score-player neighbors me) (player-role me))))
(check-expect (update-player-score 0 0 RANDOM-WORLD1)
              (make-player 3 7 1 false))
(check-expect (update-player-score 1 1 RANDOM-WORLD1)
              (make-player 9 9 0 false))

;;update-strategies : World -> World
;;Updates the strategies of all the players based on how well they
;; did versus how well their neighbors did
(define (update-strategies w)
  (map (λ (rowi) (map (λ (coli) (update-player-strategy rowi coli w))
                      (build-list (length w) identity)))
       (build-list (length w) identity)))
(check-expect
 (map (λ (row) (map player-propose row))
      (update-strategies (update-scores RANDOM-WORLD1)))
 '((3 3 3) (6 9 3) (3 10 3)))
(check-expect
 (map (λ (row) (map player-respond row))
      (update-strategies (update-scores RANDOM-WORLD1)))
 '((7 3 6) (6 6 10) (3 6 1)))
(check-expect (update-strategies '()) '())

;;update-player-strategy : Nat Nat World -> Player
;;Update the player's strategy as necessary
(define (update-player-strategy rowi coli w)
  (local [(define neighbors (get-player-neighbors rowi coli w))
          (define me (list-ref (list-ref w rowi) coli))
          (define best-proposer (get-best-proposer neighbors me))
          (define best-responder (get-best-responder neighbors me))]
    (if (player-role me)
        (make-player (player-propose best-proposer)
                     (player-respond me)
                     (player-score me) (random-bool))
        (make-player (player-propose me)
                     (player-respond best-responder)
                     (player-score me) (random-bool)))))
(check-expect (player-respond (update-player-strategy 1 1 (update-scores RANDOM-WORLD1))) 6)
(check-expect (player-propose (update-player-strategy 1 2 (update-scores RANDOM-WORLD1))) 3)

;;get-player-neighbors : Nat Nat World -> [List Player]
;;Get the player's neighbors
(define (get-player-neighbors rowi coli w)
  (local [(define gs (length w))
          (define lessy (modulo (sub1 rowi) gs))
          (define morey (modulo (add1 rowi) gs))
          (define lessx (modulo (sub1 coli) gs))
          (define morex (modulo (add1 coli) gs))]
    (list (list-ref (list-ref w lessy) lessx)
          (list-ref (list-ref w lessy) coli)
          (list-ref (list-ref w lessy) morex)
          (list-ref (list-ref w rowi) lessx)
          (list-ref (list-ref w rowi) morex)
          (list-ref (list-ref w morey) lessx)
          (list-ref (list-ref w morey) coli)
          (list-ref (list-ref w morey) morex))))
(check-expect (get-player-neighbors 1 1 '((1 2 3) (4 5 6) (7 8 9)))
              '(1 2 3 4 6 7 8 9))

;;get-best-proposer : [List Player] Player -> Player
;;Gets the proposer with the best score
;;If there are no proposers, return the player
(define (get-best-proposer others me)
  (foldr (λ (p sofar)
           (if (and (player-role p) (> (player-score p) (player-score sofar))) p sofar))
         me others))
(check-expect (get-best-proposer empty (make-player 9 5 10 false)) (make-player 9 5 10 false))
(check-expect (get-best-proposer
               (list (make-player 9 5 10 false)
                     (make-player 9 5 8 true)
                     (make-player 6 3 7 true))
               (make-player 8 2 2 false))
              (make-player 9 5 8 true))

;;get-best-responder : [List Player] Player -> Player
;;Gets the responder with the best score
;;If there are no responders, return the player
(define (get-best-responder others me)
  (foldr (λ (p sofar)
           (if (and (not (player-role p)) (> (player-score p) (player-score sofar))) p sofar))
         me others))
(check-expect (get-best-responder empty (make-player 9 5 10 false)) (make-player 9 5 10 false))
(check-expect (get-best-responder
               (list (make-player 9 5 6 false)
                     (make-player 9 5 8 true)
                     (make-player 6 3 7 true))
               (make-player 8 2 2 false))
              (make-player 9 5 6 false))

;;score-player : [List Player] Player -> Number
;;Score the player against all player's of the opposite type
(define (score-player others me)
  (local [(define opposites (filter (λ (x) (xor (player-role me) (player-role x))) others))]
    (if (zero? (length opposites)) 0
        (floor (/ (foldr (λ (p sofar) (+ (battle p me) sofar))
                         0 opposites) (length opposites))))))
(check-expect (score-player
               (list (make-player 6 9 0 false)
                     (make-player 9 9 0 false)
                     (make-player 9 10 0 true)
                     (make-player 9 3 0 true)
                     (make-player 3 1 0 true)
                     (make-player 3 7 0 false)
                     (make-player 8 3 0 true)
                     (make-player 9 6 0 true))
               (make-player 10 6 0 false)) 1)
(check-expect (score-player '() (make-player 10 6 0 false)) 0)

;;player=? : Player Player -> Boolean
;;Do these 2 players have the same strategies and the same role?
(define (player=? a b)
  (and (= (player-propose a) (player-propose b))
       (= (player-respond a) (player-respond b))
       (boolean=? (player-role a) (player-role b))))
(check-expect (player=? (make-player 4 2 0 false) (make-player 4 2 10 false)) true)
(check-expect (player=? (make-player 4 2 10 false) (make-player 4 2 0 true)) false)
(check-expect (player=? (make-player 4 2 10 true) (make-player 6 2 5 true)) false)
(check-expect (player=? (make-player 4 2 10 false) (make-player 4 6 10 false)) false)

;;xor : Boolean Boolean -> Boolean
(define (xor a b)
  (not (boolean=? a b)))
(check-expect (xor true false) true)
(check-expect (xor false true) true)
(check-expect (xor false false) false)
(check-expect (xor true true) false)

;;battle : Player Player -> Number
;;Find my score against this player
;;ASSUMPTION: The other player has the opposite role
(define (battle other me)
  (cond [(and (player-role me)
              (>= (- 10 (player-propose me))
                 (player-respond other)))
         (player-propose me)]
        [(and (not (player-role me))
              (>= (- 10 (player-propose other))
                 (player-respond me)))
         (- 10 (player-propose other))]
        [else 0]))
(check-expect (battle (make-player 8 2 5 false) (make-player 9 3 4 true)) 0)
(check-expect (battle (make-player 6 1 10 true) (make-player 4 4 10 false)) 4)

;;draw-world : World -> Image
;;Renders the grid as an image
(define (draw-world w)
  (overlay
   (foldr (λ (row sofar) (above (draw-row row) sofar))
          empty-image w) (empty-scene SCENE-SIZE SCENE-SIZE)))
(check-expect (draw-world '()) (empty-scene SCENE-SIZE SCENE-SIZE))
(check-expect
 (draw-world RANDOM-WORLD1)
 (overlay
  (above (draw-row
          (list (make-player 3 7 0 false) (make-player 8 3 0 true) (make-player 9 6 0 true)))
         (draw-row
          (list (make-player 6 9 0 false) (make-player 9 9 0 false) (make-player 9 10 0 true)))
         (draw-row
          (list (make-player 9 3 0 true) (make-player 10 6 0 false) (make-player 3 1 0 true))))
  (empty-scene SCENE-SIZE SCENE-SIZE)))

;;draw-row : [List Player] -> Image
;;Renders the row as an image
(define (draw-row lop)
  (foldr (λ (cell sofar) (beside (draw-cell cell) sofar))
         empty-image lop))
(check-expect (draw-row '()) empty-image)
(check-expect (draw-row (first RANDOM-WORLD1))
              (beside (draw-cell (make-player 3 7 0 false))
                      (draw-cell (make-player 8 3 0 true))
                      (draw-cell (make-player 9 6 0 true))))

;;draw-cell : Player -> Image
;;Renders the player as an image
(define (draw-cell p)
  (overlay (right-triangle CELL-SIZE CELL-SIZE "solid" (get-color (player-propose p)))
           (flip-horizontal
            (flip-vertical
             (right-triangle CELL-SIZE CELL-SIZE "solid" (get-color (player-respond p)))))))
(check-expect
 (draw-cell (make-player 6 4 0 false))
 (overlay (right-triangle CELL-SIZE CELL-SIZE "solid" 'Indigo)
          (flip-horizontal
           (flip-vertical
            (right-triangle CELL-SIZE CELL-SIZE "solid" 'Green)))))
(check-expect
 (draw-cell (make-player 8 1 10 true))
 (overlay (right-triangle CELL-SIZE CELL-SIZE "solid" 'HotPink)
          (flip-horizontal
           (flip-vertical
            (right-triangle CELL-SIZE CELL-SIZE "solid" 'Red)))))

;;get-color : Number -> Color
(define (get-color n)
  (list-ref COLORS (sub1 n)))
(check-expect (get-color 1) 'Red)
(check-expect (get-color 5) 'Blue)

;;when-to-stop : World -> Boolean
;;Determines if everyone has the same strategy
(define (when-to-stop w)
  (local [(define first-strat (first (first w)))]
    (andmap (λ (row) (andmap (λ (cell) (and (= (player-propose cell) (player-propose first-strat))
                                            (<= (player-respond cell)
                                                (- 10 (player-propose first-strat)))))
                             row)) w)))
(check-expect (when-to-stop RANDOM-WORLD1) false)
(check-expect
 (when-to-stop
  (list (list (make-player 6 2 6 false)
              (make-player 6 4 3 true)
              (make-player 6 3 10 false))
        (list (make-player 6 1 5 true)
              (make-player 6 2 3 false)
              (make-player 6 3 4 true))
        (list (make-player 6 4 3 false)
              (make-player 6 3 2 false)
              (make-player 6 1 9 true)))) true)

#|------------------------------------------------RUN-----------------------------------------------|#
(main)