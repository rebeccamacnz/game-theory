;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname CostlySignaling) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

#|--- DATA DEFINITIONS ---|#

;; A Player is a (make-player Boolean Strategy Strategy Nat)
(define-struct player (role sender receiver score))
; - where the Boolean is true if the player is the sender and false if they are the receiver
; - the first Strategy tells the player what to do if they are the sender
; - the second Strategy tells the player what to do if they are the receiver
; - and the Nat is the total score of the player for this round

;; A Strategy is a (make-strat Boolean Boolean)
(define-struct strat (a b))
; - where the first Boolean tells the player what to do in the first case
;   - for a Sender this is what to do in a "true" world state
;   - for a Receiver this is what to do if you receive a "true" signal
; - and the second Boolean tells the player what to do in the second case
;   - for a Sender this is what to do in a "false" world state
;   - for a Receiver this is what to do if you receive a "false" signal (no signal)

;A [Grid X] is a [List-of [List-of X]] where each inner list is the same length

;;A World is a (make-world Boolean [Grid Player])
(define-struct world (state grid))
; - where the Boolean is the state of the world
; - and the [Grid Player] is all the players

#|--- CONSTANTS ---|#
(define GRID-WIDTH 25)
(define GRID-HEIGHT 25)
(define CELL-SIZE 22)

(define BG (empty-scene (* GRID-WIDTH CELL-SIZE)
                        (* GRID-HEIGHT CELL-SIZE)))
(define TRUE-COLOR "green")
(define FALSE-COLOR "red")
(define BETWEEN-COLOR "white")

(define REWARD 100)
(define COST 99)


(define TESTPLAYER1
  (make-player false (make-strat false true)
               (make-strat true true) 0))
(define TESTPLAYER2
  (make-player true (make-strat false true)
               (make-strat false false) 0))
(define TESTPLAYER3
  (make-player true (make-strat true true)
               (make-strat true false) 0))
(define TESTPLAYER4
  (make-player false (make-strat true true)
               (make-strat false false) 0))
(define TESTPLAYER5
  (make-player true (make-strat false true)
               (make-strat true false) 0))
(define TESTPLAYER6
  (make-player true (make-strat true false)
               (make-strat false true) 0))
(define TESTPLAYER7
  (make-player false (make-strat true false)
               (make-strat true false) 0))
(define TESTPLAYER8
  (make-player true (make-strat true true)
               (make-strat false false) 0))
(define TESTPLAYER9
  (make-player false (make-strat false true)
               (make-strat true false) 0))

(define TESTWORLD1
  (make-world true
              (list (list TESTPLAYER1 TESTPLAYER2 TESTPLAYER3)
                    (list TESTPLAYER4 TESTPLAYER5 TESTPLAYER6)
                    (list TESTPLAYER7 TESTPLAYER8 TESTPLAYER9))))

#|--- MAIN FUNCTIONS ---|#
;;main : Nat Nat -> World
;;Runs the simulation with a world with the given width and height
(define (main w h)
  (big-bang (initial-world w h)
            (on-tick tick-world)
            (to-draw draw-world)
            (stop-when stop? final-scene)))

;;initial-world : Nat Nat -> World
;;Produces an initial world with the given width and height
(define (initial-world w h)
  (make-world (random-boolean)
              (random-grid w h)))
(check-expect (world? (initial-world (random 20) (random 20))) true)
(check-expect (boolean? (world-state (initial-world (random 20) (random 20)))) true)
(check-expect (length (world-grid (initial-world (random 20) 20))) 20)
(check-expect (map length (world-grid (initial-world 18 7)))
              (make-list 7 18))

;;random-grid : Nat Nat -> [Grid Player]
;;Produces an initial grid of players with the given width and height
(define (random-grid w h)
  (build-list h (λ (ri) (build-list w (λ (ci) (random-player))))))
(check-expect (length (random-grid (random 20) 15)) 15)
(check-expect (map length (random-grid 17 8))
              (make-list 8 17))

;;random-player : -> Player
;;Produces a player with a random strategy
(define (random-player)
  (make-player (random-boolean) (random-strategy) (random-strategy) 0))
(check-expect (player? (random-player)) true)
(check-expect (boolean? (player-role (random-player))) true)
(check-expect (strat? (player-sender (random-player))) true)
(check-expect (strat? (player-receiver (random-player))) true)
(check-expect (player-score (random-player)) 0)

;;random-strategy : -> Strategy
;;Produces a random strategy
(define (random-strategy)
  (make-strat (random-boolean) (random-boolean)))
(check-expect (strat? (random-strategy)) true)
(check-expect (boolean? (strat-a (random-strategy))) true)
(check-expect (boolean? (strat-b (random-strategy))) true)

;;tick-world : World -> World
;;Update the world
(define (tick-world w)
  (update-state (update-strategies (update-scores w))))
(check-expect (world? (tick-world TESTWORLD1)) true)
(check-expect (boolean? (world-state (tick-world TESTWORLD1))) true)
(check-expect (world-grid (tick-world (make-world (random-boolean) '()))) '())
#;(check-expect (map (λ (r) (map player-sender r)) (world-grid (tick-world TESTWORLD1)))
              (list (list (make-strat false true)
                          (make-strat false true)
                          (make-strat true true))
                    (list (make-strat true true)
                          (make-strat false true)
                          (make-strat true false))
                    (list (make-strat true false)
                          (make-strat true true)
                          (make-strat false true))))
(check-expect (map (λ (r) (map player-receiver r)) (world-grid (tick-world TESTWORLD1)))
              (list (list (make-strat true true)
                          (make-strat false false)
                          (make-strat true false))
                    (list (make-strat true true)
                          (make-strat true false)
                          (make-strat false true))
                    (list (make-strat true true)
                          (make-strat false false)
                          (make-strat true true))))
 
;;update-scores : World -> World
;;Update the players' scores
(define (update-scores w)
  (make-world (world-state w)
              (update-grid-scores (world-state w) (world-grid w))))
(check-expect (update-scores (make-world false '()))
              (make-world false '()))
(check-expect (foldr (λ (r sofar) (append (map player-score r) sofar)) '()
                     (world-grid (update-scores TESTWORLD1)))
              (list (* 5 REWARD) REWARD (- (* 3 REWARD) (* 4 COST))
                    0 REWARD (- (* 3 REWARD) (* 4 COST))
                    (* 3 REWARD) (- (* 3 REWARD) (* 4 COST)) (* 3 REWARD)))

;;update-grid-scores : Boolean [Grid Player] -> [Grid Player]
;;Update the score of each player on the grid (given the current worldstate)
(define (update-grid-scores ws grid)
  (grid-index-map grid (λ (ri ci) (update-player-score ws grid ri ci))))
(check-expect (update-grid-scores (random-boolean) '()) '())
(check-expect (foldr (λ (r sofar) (append (map player-score r) sofar)) '()
                     (update-grid-scores true (world-grid TESTWORLD1)))
              (list (* 5 REWARD) REWARD (- (* 3 REWARD) (* 4 COST))
                    0 REWARD (- (* 3 REWARD) (* 4 COST))
                    (* 3 REWARD) (- (* 3 REWARD) (* 4 COST)) (* 3 REWARD)))

;;update-player-score : Boolean [Grid Player] Nat Nat -> Player
;;Update the score of the player with this row and column index
(define (update-player-score ws grid ri ci)
  (local [(define me (get-grid-element grid ri ci))
          (define neighbors (filter (λ (n) (xor (player-role n) (player-role me)))
                                    (get-grid-neighbors grid ri ci)))]
    (make-player (player-role me)
                 (player-sender me)
                 (player-receiver me)
                 (foldr (λ (n sofar) (+ (find-my-score ws me n) sofar)) 0
                        neighbors))))
(check-expect (update-player-score true (world-grid TESTWORLD1) 1 1)
              (make-player true (make-strat false true) (make-strat true false) REWARD))
(check-expect (update-player-score true (world-grid TESTWORLD1) 0 2)
              (make-player true (make-strat true true)
                           (make-strat true false) (- (* 3 REWARD) (* 4 COST))))

;;find-my-score : Boolean Player Player -> Nat
;;Find the score for player1 against player2 given the world state
;;ASSUMPTION: The players have opposite roles
(define (find-my-score ws player1 player2)
  (if (player-role player1)
      (find-sender-score ws (player-sender player1) (player-receiver player2))
      (find-receiver-score ws (player-receiver player1) (player-sender player2))))
(check-expect (find-my-score true (make-player false (make-strat false true)
                                               (make-strat true true) 0)
                             (make-player true (make-strat true true)
                                          (make-strat false false) 0))
              REWARD)
(check-expect (find-my-score false (make-player true (make-strat true true)
                                                (make-strat true false) 0)
                             (make-player false (make-strat true false)
                                          (make-strat false true) 0))
              (- REWARD COST))

;;find-sender-score : Boolean Strategy Strategy -> Nat
;;Find the score for player1 who is the sender
(define (find-sender-score ws player1 player2)
  (cond [(or (and ws (not (strat-a player1)) (strat-b player2))
             (and (not ws) (not (strat-b player1)) (not (strat-b player2))))
         REWARD]
        [(or (and ws (strat-a player1) (strat-a player2))
             (and (not ws) (strat-b player1) (not (strat-a player2))))
         (- REWARD COST)]
        [(or (and ws (strat-a player1) (not (strat-a player2)))
             (and (not ws) (strat-b player1) (strat-a player2)))
         (- COST)]
        [else 0]))
(check-expect (find-sender-score false (make-strat true false)
                                 (make-strat true false)) REWARD)
(check-expect (find-sender-score true (make-strat true true)
                                 (make-strat true true)) (- REWARD COST))
(check-expect (find-sender-score false (make-strat false true)
                                 (make-strat true false)) (- COST))
(check-expect (find-sender-score true (make-strat false true)
                                 (make-strat true false)) 0)

;;find-receiver-score : Boolean Strategy Strategy -> Nat
;;Find the score for player1 who is the receiver
(define (find-receiver-score ws player1 player2)
  (cond [(or (and ws (or (and (strat-a player2) (strat-a player1))
                         (and (not (strat-a player2)) (strat-b player1))))
             (and (not ws) (or (and (strat-b player2) (not (strat-a player1)))
                               (and (not (strat-b player2)) (not (strat-b player1))))))
         REWARD]
        [else 0]))
(check-expect (find-receiver-score true (make-strat true false) (make-strat true false)) REWARD)
(check-expect (find-receiver-score false (make-strat true false) (make-strat true false)) REWARD)
(check-expect (find-receiver-score true (make-strat false false) (make-strat false true)) 0)

;;update-strategies : World -> World
;;Update the players' strategies
(define (update-strategies w)
  (make-world (world-state w)
              (update-grid-strategies (world-grid w))))
(check-expect (update-strategies TESTWORLD1) TESTWORLD1)
(check-expect (update-strategies (make-world false '())) (make-world false '()))
#;(check-expect
 (update-strategies
  (update-scores TESTWORLD1))
 (make-world
  true
  (list (list (make-player false (make-strat false true) (make-strat true true) (* 5 REWARD))
              (make-player true (make-strat false true) (make-strat false false) REWARD)
              (make-player true (make-strat true true) (make-strat true false)
                           (- (* 3 REWARD) (* 4 COST))))
        (list (make-player false (make-strat true true) (make-strat true true) 0)
              (make-player true (make-strat false true) (make-strat true false) REWARD)
              (make-player true (make-strat true false) (make-strat false true)
                           (- (* 3 REWARD) (* 4 COST))))
        (list (make-player false (make-strat true false) (make-strat true true) (* 3 REWARD))
              (make-player true (make-strat true true) (make-strat false false)
                           (- (* 3 REWARD) (* 4 COST)))
              (make-player false (make-strat false true) (make-strat true true) (* 3 REWARD))))))

;;update-grid-strategies : [Grid Player] -> [Grid Player]
;;Update each player's strategy to reflect the best strategy in their neighborhood
(define (update-grid-strategies g)
  (grid-index-map g (λ (ri ci) (update-player-strategy g ri ci))))
(check-expect (update-grid-strategies '()) '())
(check-expect (update-grid-strategies (world-grid TESTWORLD1)) (world-grid TESTWORLD1))
#;(check-expect
 (update-grid-strategies (world-grid (update-scores TESTWORLD1)))
 (list (list (make-player false (make-strat false true) (make-strat true true) (* 5 REWARD))
             (make-player true (make-strat false true) (make-strat false false) REWARD)
             (make-player true (make-strat true true) (make-strat true false)
                          (- (* 3 REWARD) (* 4 COST))))
       (list (make-player false (make-strat true true) (make-strat true true) 0)
             (make-player true (make-strat false true) (make-strat true false) REWARD)
             (make-player true (make-strat true false) (make-strat false true)
                          (- (* 3 REWARD) (* 4 COST))))
       (list (make-player false (make-strat true false) (make-strat true true) (* 3 REWARD))
             (make-player true (make-strat true true) (make-strat false false)
                          (- (* 3 REWARD) (* 4 COST)))
             (make-player false (make-strat false true) (make-strat true true) (* 3 REWARD)))))

;;update-player-strategy : [Grid Player] Nat Nat -> Player
;;Update the strategy of the player at this row and column index
(define (update-player-strategy g ri ci)
  (local [(define me (get-grid-element g ri ci))
          (define neighbors
            (filter (λ (n) (boolean=? (player-role me) (player-role n)))
                    (get-grid-neighbors g ri ci)))]
    (make-player (player-role me)
                 (if (player-role me)
                     (find-best-strat neighbors me player-sender)
                     (player-sender me))
                 (if (not (player-role me))
                     (find-best-strat neighbors me player-receiver)
                     (player-receiver me))
                 (player-score me))))
(check-expect (update-player-strategy (world-grid (update-scores TESTWORLD1)) 2 0)
              (make-player false (make-strat true false) (make-strat true true) (* 3 REWARD)))
(check-expect (update-player-strategy (world-grid (update-scores TESTWORLD1)) 0 1)
              (make-player true (make-strat false true) (make-strat false false) REWARD))

;;find-best-strat : [List Player] Player [Player -> Strategy] -> Strategy
;;Find the highest-scoring player and takes their strategy
(define (find-best-strat lop base getstrat)
  (getstrat
   (foldr (λ (p sofar) (if (> (player-score p) (player-score sofar)) p sofar))
          base lop)))
(check-expect
 (find-best-strat '() (make-player true (make-strat true false) (make-strat false true) 0)
                  player-sender)
 (make-strat true false))
(check-expect
 (find-best-strat
  (list (make-player true (make-strat true false) (make-strat false true) 5)
        (make-player true (make-strat false true) (make-strat true false) 10))
  (make-player true (make-strat false false) (make-strat true true) 5) player-sender)
 (make-strat false true))
(check-expect
 (find-best-strat
  (list (make-player false (make-strat true false) (make-strat false true) 1)
        (make-player false (make-strat false true) (make-strat true false) 2))
  (make-player false (make-strat false false) (make-strat true true) 5) player-receiver)
 (make-strat true true))

;;update-state : World -> World
;;Give the world a new random world state and each player a new random role
(define (update-state w)
  (make-world (random-boolean)
              (update-grid-roles (world-grid w))))
(check-expect (world? (update-state TESTWORLD1)) true)
(check-expect (boolean? (world-state (update-state TESTWORLD1))) true)
(check-expect
 (grid-index-andmap (world-grid TESTWORLD1)
                    (λ (ri ci) (same-strategies?
                                (get-grid-element (world-grid TESTWORLD1) ri ci)
                                (get-grid-element (world-grid (update-state TESTWORLD1)) ri ci))))
 true)

;;same-strategies? : Player Player -> Boolean
;;Do these players have the same strategies?
(define (same-strategies? p1 p2)
  (and (strategy=? (player-sender p1) (player-sender p2))
       (strategy=? (player-receiver p1) (player-receiver p2))))
(check-expect
 (same-strategies?
  (make-player false (make-strat false true) (make-strat true false) 0)
  (make-player true (make-strat true false) (make-strat false true) 10)) false)
(check-expect
 (same-strategies?
  (make-player true (make-strat false true) (make-strat true false) 5)
  (make-player false (make-strat false true) (make-strat true false) 13)) true)

;;strategy=? : Strategy Strategy -> Boolean
;;Are these strategies the same?
(define (strategy=? s1 s2)
  (and (boolean=? (strat-a s1) (strat-a s2))
       (boolean=? (strat-b s1) (strat-b s2))))
(check-expect (strategy=? (make-strat false true) (make-strat false false)) false)
(check-expect (strategy=? (make-strat true false) (make-strat true false)) true)

;;update-grid-roles : [Grid Player] -> [Grid Player]
;;Assigns each player a new role at random
(define (update-grid-roles g)
  (grid-map g update-player-role))
(check-expect (update-grid-roles '()) '())
(check-expect (length (update-grid-roles (world-grid TESTWORLD1))) 3)
(check-expect (grid-index-andmap
               (world-grid TESTWORLD1)
               (λ (ri ci) (same-strategies?
                           (get-grid-element (world-grid TESTWORLD1) ri ci)
                           (get-grid-element (update-grid-roles (world-grid TESTWORLD1)) ri ci))))
              true)

;;update-player-role : Player -> Player
;;Assign this player a new role at random
(define (update-player-role p)
  (make-player (random-boolean)
               (player-sender p)
               (player-receiver p)
               (player-score p)))
(check-expect (player? (update-player-role TESTPLAYER1)) true)
(check-expect (player-sender (update-player-role TESTPLAYER2))
              (player-sender TESTPLAYER2))
(check-expect (player-receiver (update-player-role TESTPLAYER3))
              (player-receiver TESTPLAYER3))
(check-expect (player-score (update-player-role TESTPLAYER4)) 0)
                
;;draw-world : World -> Image
;;Render the grid of players as an image
(define (draw-world w)
  (draw-grid (world-grid w)))
(check-expect (draw-world (make-world false '())) BG)
#;(check-expect (draw-world TESTWORLD1) ...)

;;draw-grid : [Grid Player] -> Image
;;Draw the grid as an image
(define (draw-grid g)
  (overlay
   (grid-foldr g above (λ (p rowsofar) (beside (draw-player p) rowsofar)) empty-image empty-image)
   BG))
(check-expect (draw-grid '()) BG)
#;(check-expect (draw-grid (world-grid TESTWORLD1)) ...)

;;draw-player : Player -> Image
;;Draw the player as an image
; - top row = sender strategy
; - bottom row = receiver strategy
(define (draw-player p)
  (overlay
   (above (draw-strategy (player-sender p))
          (draw-strategy (player-receiver p)))
   (square 22 "solid" BETWEEN-COLOR)))
(check-expect
 (draw-player TESTPLAYER1)
 (overlay
  (above (draw-strategy (make-strat false true))
         (draw-strategy (make-strat true true)))
  (square 22 "solid" BETWEEN-COLOR)))

;;draw-strategy : Strategy -> Image
;;Draw the strategy as an image
(define (draw-strategy s)
  (beside (overlay (square 9 "solid" (boolean->color (strat-a s)))
                   (square 10 "solid" "black"))
          (overlay (square 9 "solid" (boolean->color (strat-b s)))
                   (square 10 "solid" "black"))))
(check-expect (draw-strategy (make-strat true false))
              (beside (overlay (square 9 "solid" TRUE-COLOR)
                               (square 10 "solid" "black"))
                      (overlay (square 9 "solid" FALSE-COLOR)
                               (square 10 "solid" "black"))))

;;boolean->color : Boolean -> Color
;;Produces the color we are using for this boolean
(define (boolean->color b)
  (if b TRUE-COLOR FALSE-COLOR))
(check-expect (boolean->color true) TRUE-COLOR)
(check-expect (boolean->color false) FALSE-COLOR)

;;stop? : World -> Boolean
;;Determines if the simulation is over
(define (stop? w)
  (or (empty? (world-grid w))
      (local [(define player1 (first (first (world-grid w))))]
        (grid-foldr (world-grid w) (λ (x y) (and x y))
                    (λ (p rowsofar) (and (same-strategies? player1 p) rowsofar))
                    true true))))
(check-expect (stop? (make-world false '())) true)
(check-expect (stop? TESTWORLD1) false)

;;final-scene : World -> Image
;;Produce text of the strategy that "won"
(define (final-scene w)
  (cond [(empty? (world-grid w)) BG]
        [else (overlay (text (final-strategy-text (world-grid w)) 45 "black") BG)]))
(check-expect (final-scene (make-world true '())) BG)
(check-expect (final-scene TESTWORLD1)
              (overlay (text "FTTT" 45 "black") BG))

;;final-strategy-text : [Grid Player] -> String
;;Produces the string that corresponds to the strategy of the first player in the grid
(define (final-strategy-text g)
  (string-append
   (strategy->string (player-sender (first (first g))))
   (strategy->string (player-receiver (first (first g))))))
(check-expect (final-strategy-text (world-grid TESTWORLD1)) "FTTT")

;;strategy->string : Strategy -> String
;;Produces the string representation of this strategy
(define (strategy->string s)
  (string-append (if (strat-a s) "T" "F")
                 (if (strat-b s) "T" "F")))
(check-expect (strategy->string (make-strat true true)) "TT")
(check-expect (strategy->string (make-strat true false)) "TF")
(check-expect (strategy->string (make-strat false true)) "FT")
(check-expect (strategy->string (make-strat false false)) "FF")

#|--- GRID FUNCTIONS ---|#

;;get-grid-element : [Grid X] Nat Nat -> X
;;Get the element with this row and column index
(define (get-grid-element grid ri ci)
  (list-ref (list-ref grid ri) ci))
(check-expect (get-grid-element '((a b c) (d e f) (g h i)) 1 1) 'e)
(check-expect (get-grid-element '((a b c) (d e f) (g h i)) 2 0) 'g)

;;get-grid-neighbors : [Grid X] Nat Nat -> [List X]
;;Get the neighbors of the element with this row and column index
;;Assumption: The grid has an element at this location
(define (get-grid-neighbors grid ri ci)
  (local [(define gh (length grid))
          (define gw (length (first grid)))
          (define prevrow (modulo (sub1 ri) gh))
          (define nextrow (modulo (add1 ri) gh))
          (define prevcol (modulo (sub1 ci) gw))
          (define nextcol (modulo (add1 ci) gw))]
    (list (get-grid-element grid prevrow prevcol)
          (get-grid-element grid prevrow ci)
          (get-grid-element grid prevrow nextcol)
          (get-grid-element grid ri prevcol)
          (get-grid-element grid ri nextcol)
          (get-grid-element grid nextrow prevcol)
          (get-grid-element grid nextrow ci)
          (get-grid-element grid nextrow nextcol))))
(check-expect (get-grid-neighbors '((a b c) (d e f) (g h i)) 1 1)
              '(a b c d f g h i))
(check-expect (get-grid-neighbors '((a b c) (d e f) (g h i)) 0 2)
              '(h i g b a e f d))

;;grid-map : [Grid X] [X -> Y] -> [Grid Y]
;;Map this function over the elements in the grid
(define (grid-map g f)
  (map (λ (r) (map (λ (c) (f c)) r)) g))
(check-expect (grid-map '((1 2 3) (4 5 6) (7 8 9)) sqr)
              '((1 4 9) (16 25 36) (49 64 81)))
(check-expect (grid-map '() add1) '())
(check-expect (grid-map '((a b c) (d e f) (g h i)) (λ (s) (symbol=? s 'd)))
              (list (list false false false)
                    (list true false false)
                    (list false false false)))

;;grid-index-map : [Grid X] [Nat Nat -> Y] -> [Grid Y]
;;Map this function over the indices in the grid
(define (grid-index-map g f)
  (local [(define gh (length g))
          (define gw (if (empty? g) 0 (length (first g))))]
    (map (λ (ri) (map (λ (ci) (f ri ci)) (build-list gw identity)))
         (build-list gh identity))))
(check-expect (grid-index-map '((a b c) (d e f) (g h i)) +)
              '((0 1 2) (1 2 3) (2 3 4)))
(check-expect (grid-index-map '() -) '())

;;grid-index-andmap : [Grid X] [Nat Nat -> Boolean] -> Boolean
;;Do all the indices in this grid pass the given predicate?
(define (grid-index-andmap g pred)
  (local [(define gh (length g))
          (define gw (if (empty? g) 0 (length (first g))))]
    (andmap (λ (ri) (andmap (λ (ci) (pred ri ci)) (build-list gw identity)))
            (build-list gh identity))))
(check-expect (grid-index-andmap '((a b c) (d e f) (g h i)) =) false)
(check-expect (grid-index-andmap '() >) true)

;;grid-foldr : [Grid X] [Y Y -> Y] [X Y -> Y] Y Y -> Y
;;Very abstract...
(define (grid-foldr g rcombine ccombine cbase rbase)
  (foldr (λ (r sofar) (rcombine (foldr ccombine cbase r) sofar)) rbase g))

#|--- AUXILLARY FUNCTIONS ---|#

;;random-boolean : -> Boolean
;;Produces either true or false
(define (random-boolean)
  (zero? (random 2)))
(check-expect (boolean? (random-boolean)) true)

;;xor : Boolean Boolean -> Boolean
;;Produces true if the two booleans are different
(define (xor a b)
  (not (boolean=? a b)))
(check-expect (xor true true) false)
(check-expect (xor true false) true)
(check-expect (xor false true) true)
(check-expect (xor false false) false)