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
(define COLORS '(Red Orange Yellow Green Olive Blue Indigo Purple HotPink Chocolate Black))
#|---------------------------------------------FUNCTIONS--------------------------------------------|#

;;main : -> World
(define (main)
  (big-bang (initial-world)
            (on-tick tick-world .3)
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

;;random-bool : -> Boolean
;;Produces true or false randomly
(define (random-bool)
  (zero? (random 2)))

;;tick-world : World -> World
;;Update the players' strategies as necessary
(define (tick-world w)
  (update-strategies (update-scores w)))
#|TEST THIS|#

;;update-scores : World -> World
;;Updates the scores of all the players based on how well they did against their neighbors
(define (update-scores w)
  (map (λ (rowi) (map (λ (coli) (update-player-score rowi coli w))
                      (build-list GRID-SIZE identity)))
       (build-list GRID-SIZE identity)))
#|TEST THIS|#

;;update-player-score : Nat Nat World -> Player
;;Updates the score of this player based on how well they did against their neighbors
(define (update-player-score rowi coli w)
  (local [(define neighbors (get-player-neighbors rowi coli w))
          (define me (list-ref (list-ref w rowi) coli))]
    (make-player (player-propose me) (player-respond me)
                 (score-player neighbors me) (player-role me))))
#|TEST THIS|#

;;update-strategies : World -> World
;;Updates the strategies of all the players based on how well they
;; did versus how well their neighbors did
(define (update-strategies w)
  (map (λ (rowi) (map (λ (coli) (update-player-strategy rowi coli w))
                      (build-list GRID-SIZE identity)))
       (build-list GRID-SIZE identity)))
#|TEST THIS|#

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
#|TEST THIS|#

;;get-player-neighbors : Nat Nat World -> [List Player]
;;Get the player's neighbors
(define (get-player-neighbors rowi coli w)
  (local [(define cols (map (λ (x) (modulo (+ rowi x) GRID-SIZE)) (list -1 0 1)))
          (define rows (map (λ (x) (modulo (+ coli x) GRID-SIZE)) (list -1 1)))]
    (append
     (foldr (λ (rowx sofar) (append (map (λ (colx) (list-ref (list-ref w rowx) colx)) cols) sofar))
           '() rows)
     (list (list-ref (list-ref w rowi) (modulo (sub1 coli) GRID-SIZE))
           (list-ref (list-ref w rowi) (modulo (add1 coli) GRID-SIZE))))))
(check-expect (get-player-neighbors 1 1 '((1 2 3) (4 5 6) (7 8 9)))
              '(1 2 3 7 8 9 4 6))

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
        (/ (foldr (λ (p sofar) (+ (battle p me) sofar))
                  0 opposites) (length opposites)))))
#|TEST THIS|#

;;xor : Boolean Boolean -> Boolean
(define (xor a b)
  (not (boolean=? a b)))

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
  (local [(define SCENE-SIZE (+ (* GRID-SIZE CELL-SIZE) 10))]
    (overlay
     (foldr (λ (row sofar) (above (draw-row row) sofar))
            empty-image w) (empty-scene SCENE-SIZE SCENE-SIZE))))

;;draw-row : [List Player] -> Image
;;Renders the row as an image
(define (draw-row lop)
  (foldr (λ (cell sofar) (beside (draw-cell cell) sofar))
         empty-image lop))

;;draw-cell : Player -> Image
;;Renders the player as an image
(define (draw-cell p)
  (overlay (right-triangle CELL-SIZE CELL-SIZE "solid" (get-color (player-propose p)))
           (flip-horizontal
            (flip-vertical
             (right-triangle CELL-SIZE CELL-SIZE "solid" (get-color (player-respond p)))))))

;;get-color : Number -> Color
(define (get-color n)
  (list-ref COLORS (sub1 n)))

;;when-to-stop : World -> Boolean
;;Determines if everyone has the same strategy
(define (when-to-stop w)
  (local [(define first-strat (first (first w)))]
    (andmap (λ (row) (andmap (λ (cell) (and (= (player-propose cell) (player-propose first-strat))
                                            (= (player-respond cell) (player-respond first-strat))))
                             row)) w)))

#|------------------------------------------------RUN-----------------------------------------------|#
(main)