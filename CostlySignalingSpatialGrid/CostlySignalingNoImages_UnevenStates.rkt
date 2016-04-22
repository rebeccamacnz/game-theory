#lang racket
(require rackunit)
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

;;A World is one of:
; - (make-world Boolean [Grid Player])
(define-struct world (state grid))
;   - where the Boolean is the state of the world
;   - and the [Grid Player] is all the players
; - String

#|--- CONSTANTS ---|#
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
(define args (vector->list (current-command-line-arguments)))

#|PARSE ARGUMENTS|#
;;valid-args? : [List-of String] -> Boolean
;;Is this a list of four numbers?
(define (valid-args? args)
  (and (cons? args) (= (length args) 4)
       (string->number (first args))
       (string->number (second args))
       (string->number (third args))
       (string->number (fourth args))))

;;parse-args : [List-of String] -> [List-of Number]
;;Returns the list of four numbers unless the arguments are invalid
(define (parse-args args)
  (cond [(valid-args? args)
         (map (λ (s) (string->number s)) args)]
        [else (error "parse-args expected a [List-of String] but was given " args)]))

#|INITIAL WORLD|#
;;random-boolean : -> Boolean
;;Produces either true or false
(define (random-boolean)
  (zero? (random 2)))
#;(check-eqv? (boolean? (random-boolean)) true)

;;random-strategy : -> Strategy
;;Produces a random strategy
(define (random-strategy)
  (make-strat (random-boolean) (random-boolean)))
#;(check-eqv? (strat? (random-strategy)) true)
#;(check-eqv? (boolean? (strat-a (random-strategy))) true)
#;(check-eqv? (boolean? (strat-b (random-strategy))) true)

;;random-player : -> Player
;;Produces a player with a random strategy
(define (random-player)
  (make-player (random-boolean) (random-strategy) (random-strategy) 0))
#;(check-eqv? (player? (random-player)) true)
#;(check-eqv? (boolean? (player-role (random-player))) true)
#;(check-eqv? (strat? (player-sender (random-player))) true)
#;(check-eqv? (strat? (player-receiver (random-player))) true)
#;(check-eqv? (player-score (random-player)) 0)

;;random-grid : Nat Nat -> [Grid Player]
;;Produces an initial grid of players with the given width and height
(define (random-grid w h)
  (build-list h (λ (ri) (build-list w (λ (ci) (random-player))))))
#;(check-eqv? (length (random-grid (random 20) 15)) 15)
#;(check-eqv? (map length (random-grid 17 8))
              (make-list 8 17))

;;initial-world : Nat Nat -> World
;;Creates the initial world with a grid of this width and height
(define (initial-world w h)
  (make-world (random-boolean)
              (random-grid w h)))
#;(check-eqv? (world? (initial-world (random 20) (random 20))) true)
#;(check-eqv? (boolean? (world-state (initial-world (random 20) (random 20)))) true)
#;(check-eqv? (length (world-grid (initial-world (random 20) 20))) 20)
#;(check-eqv? (map length (world-grid (initial-world 18 7)))
              (make-list 7 18))
#|RUN WORLD|#
#|RUN WORLD - STOPPING|#
;;grid-foldr : [Grid X] [Y Y -> Y] [X Y -> Y] Y Y -> Y
;;Very abstract...
(define (grid-foldr g rcombine ccombine cbase rbase)
  (foldr (λ (r sofar) (rcombine (foldr ccombine cbase r) sofar)) rbase g))

;;strategy=? : Strategy Strategy -> Boolean
;;Are these strategies the same?
(define (strategy=? s1 s2)
  (and (boolean=? (strat-a s1) (strat-a s2))
       (boolean=? (strat-b s1) (strat-b s2))))
#;(check-eqv? (strategy=? (make-strat false true) (make-strat false false)) false)
#;(check-eqv? (strategy=? (make-strat true false) (make-strat true false)) true)

;;same-strategies? : Player Player -> Boolean
;;Do these players have the same strategies?
(define (same-strategies? p1 p2)
  (and (strategy=? (player-sender p1) (player-sender p2))
       (strategy=? (player-receiver p1) (player-receiver p2))))
#;(check-eqv?
 (same-strategies?
  (make-player false (make-strat false true) (make-strat true false) 0)
  (make-player true (make-strat true false) (make-strat false true) 10)) false)
#;(check-eqv?
 (same-strategies?
  (make-player true (make-strat false true) (make-strat true false) 5)
  (make-player false (make-strat false true) (make-strat true false) 13)) true)

;;non-communicative? : Strategy -> Boolean
;;Is this strategy non-communicative?
(define (non-communicative? s)
  (boolean=? (strat-a s) (strat-b s)))

;;same-bad-strategies? : Player Player -> Boolean
;;Do these players have the same non-communicative strategy?
(define (same-bad-strategies? p1 p2)
  (or (and (strategy=? (player-sender p1) (player-sender p2))
           (non-communicative? (player-sender p1)))
      (and (strategy=? (player-receiver p1) (player-receiver p2))
           (non-communicative? (player-receiver p1)))))

;;stop? : World -> Boolean
;;Determines if the simulation is over
(define (stop? w)
  (or (empty? (world-grid w))
      (local [(define player1 (first (first (world-grid w))))]
        (or
         (grid-foldr (world-grid w) (λ (x y) (and x y))
                    (λ (p rowsofar) (and (same-strategies? player1 p) rowsofar))
                    true true)
         (grid-foldr (world-grid w)
                     (λ (x y) (and x y))
                     (λ (p rowsofar) (and (same-bad-strategies? player1 p) rowsofar))
                     true true)))))
#;(check-eqv? (stop? (make-world false '())) true)
#;(check-eqv? (stop? TESTWORLD1) false)

;;strategy->string : Strategy -> String
;;Produces the string representation of this strategy
(define (strategy->string s)
  (string-append (if (strat-a s) "T" "F")
                 (if (strat-b s) "T" "F")))
#;(check-eqv? (strategy->string (make-strat true true)) "TT")
#;(check-eqv? (strategy->string (make-strat true false)) "TF")
#;(check-eqv? (strategy->string (make-strat false true)) "FT")
#;(check-eqv? (strategy->string (make-strat false false)) "FF")

;;final-strategy-text : [Grid Player] -> String
;;Produces the string that corresponds to the strategy of the first player in the grid
(define (final-strategy-text g)
  (string-append
   (strategy->string (player-sender (first (first g))))
   (strategy->string (player-receiver (first (first g))))))
#;(check-eqv? (final-strategy-text (world-grid TESTWORLD1)) "FTTT")

#|RUN WORLD - UPDATE SCORES|#
;;xor : Boolean Boolean -> Boolean
;;Produces true if the two booleans are different
(define (xor a b)
  (not (boolean=? a b)))
#;(check-eqv? (xor true true) false)
#;(check-eqv? (xor true false) true)
#;(check-eqv? (xor false true) true)
#;(check-eqv? (xor false false) false)

;;grid-index-map : [Grid X] [Nat Nat -> Y] -> [Grid Y]
;;Map this function over the indices in the grid
(define (grid-index-map g f)
  (local [(define gh (length g))
          (define gw (if (empty? g) 0 (length (first g))))]
    (map (λ (ri) (map (λ (ci) (f ri ci)) (build-list gw identity)))
         (build-list gh identity))))
#;(check-eqv? (grid-index-map '((a b c) (d e f) (g h i)) +)
              '((0 1 2) (1 2 3) (2 3 4)))
#;(check-eqv? (grid-index-map '() -) '())

;;get-grid-element : [Grid X] Nat Nat -> X
;;Get the element with this row and column index
(define (get-grid-element grid ri ci)
  (list-ref (list-ref grid ri) ci))
#;(check-eqv? (get-grid-element '((a b c) (d e f) (g h i)) 1 1) 'e)
#;(check-eqv? (get-grid-element '((a b c) (d e f) (g h i)) 2 0) 'g)

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
#;(check-eqv? (get-grid-neighbors '((a b c) (d e f) (g h i)) 1 1)
              '(a b c d f g h i))
#;(check-eqv? (get-grid-neighbors '((a b c) (d e f) (g h i)) 0 2)
              '(h i g b a e f d))

;;find-sender-score : Boolean Strategy Strategy Number Number -> Number
;;Find the score for player1 who is the sender
(define (find-sender-score ws player1 player2 reward cost)
  (cond [(and ws (not (strat-a player1)) (strat-b player2)) (* 5 reward)]
        [(and ws (strat-a player1) (strat-a player2)) (- (* 5 reward) cost)]
        [(and (not ws) (not (strat-a player1)) (not (strat-b player2))) reward]
        [(and (not ws) (strat-a player1) (not (strat-a player2))) (- reward cost)]
        [(or (and ws (strat-a player1) (not (strat-a player2)))
             (and (not ws) (strat-b player1) (strat-a player2)))
         (- cost)]
        [else 0]))
#;(check-eqv? (find-sender-score false (make-strat true false)
                                 (make-strat true false) 10 7) 10)
#;(check-eqv? (find-sender-score true (make-strat true true)
                                 (make-strat true true) 10 7) 3)
#;(check-eqv? (find-sender-score false (make-strat false true)
                                 (make-strat true false) 10 7) -7)
#;(check-eqv? (find-sender-score true (make-strat false true)
                                 (make-strat true false) 10 7) 0)

;;find-receiver-score : Boolean Strategy Strategy Number -> Nat
;;Find the score for player1 who is the receiver
(define (find-receiver-score ws player1 player2 reward)
  (cond [(or (and ws (or (and (strat-a player2) (strat-a player1))
                         (and (not (strat-a player2)) (strat-b player1))))
             (and (not ws) (or (and (strat-b player2) (not (strat-a player1)))
                               (and (not (strat-b player2)) (not (strat-b player1))))))
         reward]
        [else 0]))
#;(check-eqv? (find-receiver-score true (make-strat true false) (make-strat true false) 10) 10)
#;(check-eqv? (find-receiver-score false (make-strat true false) (make-strat true false) 5) 5)
#;(check-eqv? (find-receiver-score true (make-strat false false) (make-strat false true) 7) 0)

;;find-my-score : Boolean Player Player Number Number -> Nat
;;Find the score for player1 against player2 given the world state
;;ASSUMPTION: The players have opposite roles
(define (find-my-score ws player1 player2 reward cost)
  (if (player-role player1)
      (find-sender-score ws (player-sender player1) (player-receiver player2) reward cost)
      (find-receiver-score ws (player-receiver player1) (player-sender player2) reward)))
#;(check-eqv? (find-my-score true (make-player false (make-strat false true)
                                               (make-strat true true) 0)
                             (make-player true (make-strat true true)
                                          (make-strat false false) 0) 2 1) 2)
#;(check-eqv? (find-my-score false (make-player true (make-strat true true)
                                                (make-strat true false) 0)
                             (make-player false (make-strat true false)
                                          (make-strat false true) 0) 2 1) 1)

;;update-player-score : Boolean [Grid Player] Nat Nat Number Number -> Player
;;Update the score of the player with this row and column index
(define (update-player-score ws grid ri ci reward cost)
  (local [(define me (get-grid-element grid ri ci))
          (define neighbors (filter (λ (n) (xor (player-role n) (player-role me)))
                                    (get-grid-neighbors grid ri ci)))]
    (make-player (player-role me)
                 (player-sender me)
                 (player-receiver me)
                 (foldr (λ (n sofar) (+ (find-my-score ws me n reward cost) sofar)) 0
                        neighbors))))
#;(check-eqv? (update-player-score true (world-grid TESTWORLD1) 1 1 2 1)
              (make-player true (make-strat false true) (make-strat true false) 2))
#;(check-eqv? (update-player-score true (world-grid TESTWORLD1) 0 2 2 1)
              (make-player true (make-strat true true)
                           (make-strat true false) 2))

;;update-grid-scores : Boolean [Grid Player] Number Number -> [Grid Player]
;;Update the score of each player on the grid (given the current worldstate)
(define (update-grid-scores ws grid reward cost)
  (grid-index-map grid (λ (ri ci) (update-player-score ws grid ri ci reward cost))))
#;(check-eqv? (update-grid-scores (random-boolean) '() 0 0) '())
#;(check-eqv? (foldr (λ (r sofar) (append (map player-score r) sofar)) '()
                     (update-grid-scores true (world-grid TESTWORLD1) 2 1))
              (list 10 2 2 0 2 2 6 2 6))

;;update-scores : World Number Number -> World
;;Update the players' scores
(define (update-scores w r c)
  (make-world (world-state w)
              (update-grid-scores (world-state w) (world-grid w) r c)))
#;(check-eqv? (update-scores (make-world false '()) 0 0)
              (make-world false '()))
#;(check-eqv? (foldr (λ (r sofar) (append (map player-score r) sofar)) '()
                     (world-grid (update-scores TESTWORLD1 2 1)))
              (list 10 2 2 0 2 2 6 2 6))

#|RUN WORLD - UPDATE STRATEGIES|#
;;find-best-strat : [List Player] Player [Player -> Strategy] -> Strategy
;;Find the highest-scoring player and takes their strategy
(define (find-best-strat lop base getstrat)
  (getstrat
   (foldr (λ (p sofar) (if (> (player-score p) (player-score sofar)) p sofar))
          base lop)))
#;(check-eqv?
 (find-best-strat '() (make-player true (make-strat true false) (make-strat false true) 0)
                  player-sender)
 (make-strat true false))
#;(check-eqv?
 (find-best-strat
  (list (make-player true (make-strat true false) (make-strat false true) 5)
        (make-player true (make-strat false true) (make-strat true false) 10))
  (make-player true (make-strat false false) (make-strat true true) 5) player-sender)
 (make-strat false true))
#;(check-eqv?
 (find-best-strat
  (list (make-player false (make-strat true false) (make-strat false true) 1)
        (make-player false (make-strat false true) (make-strat true false) 2))
  (make-player false (make-strat false false) (make-strat true true) 5) player-receiver)
 (make-strat true true))

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
#;(check-eqv? (update-player-strategy (world-grid (update-scores TESTWORLD1 2 1)) 2 0)
              (make-player false (make-strat true false) (make-strat true true) 6))
#;(check-eqv? (update-player-strategy (world-grid (update-scores TESTWORLD1 2 1)) 0 1)
              (make-player true (make-strat false true) (make-strat false false) 2))

;;update-grid-strategies : [Grid Player] -> [Grid Player]
;;Update each player's strategy to reflect the best strategy in their neighborhood
(define (update-grid-strategies g)
  (grid-index-map g (λ (ri ci) (update-player-strategy g ri ci))))
#;(check-eqv? (update-grid-strategies '()) '())
#;(check-eqv? (update-grid-strategies (world-grid TESTWORLD1)) (world-grid TESTWORLD1))
#;(check-eqv?
 (update-grid-strategies (world-grid (update-scores TESTWORLD1 2 1)))
 (list (list (make-player false (make-strat false true) (make-strat true true) 10)
             (make-player true (make-strat false true) (make-strat false false) 2)
             (make-player true (make-strat true true) (make-strat true false) 2))
       (list (make-player false (make-strat true true) (make-strat true true) 0)
             (make-player true (make-strat false true) (make-strat true false) 2)
             (make-player true (make-strat true false) (make-strat false true) 2))
       (list (make-player false (make-strat true false) (make-strat true true) 6)
             (make-player true (make-strat true true) (make-strat false false) 2)
             (make-player false (make-strat false true) (make-strat true true) 6))))

;;update-strategies : World -> World
;;Update the players' strategies
(define (update-strategies w)
  (make-world (world-state w)
              (update-grid-strategies (world-grid w))))
#;(check-eqv? (update-strategies TESTWORLD1) TESTWORLD1)
#;(check-eqv? (update-strategies (make-world false '())) (make-world false '()))
#;(check-eqv?
 (update-strategies
  (update-scores TESTWORLD1 2 1))
 (make-world
  true
  (list (list (make-player false (make-strat false true) (make-strat true true) 10)
              (make-player true (make-strat false true) (make-strat false false) 2)
              (make-player true (make-strat true true) (make-strat true false) 2))
        (list (make-player false (make-strat true true) (make-strat true true) 0)
              (make-player true (make-strat false true) (make-strat true false) 2)
              (make-player true (make-strat true false) (make-strat false true) 2))
        (list (make-player false (make-strat true false) (make-strat true true) 6)
              (make-player true (make-strat true true) (make-strat false false) 2)
              (make-player false (make-strat false true) (make-strat true true) 6)))))

#|RUN WORLD - UPDATE STATE|#
;;grid-map : [Grid X] [X -> Y] -> [Grid Y]
;;Map this function over the elements in the grid
(define (grid-map g f)
  (map (λ (r) (map (λ (c) (f c)) r)) g))
#;(check-eqv? (grid-map '((1 2 3) (4 5 6) (7 8 9)) sqr)
              '((1 4 9) (16 25 36) (49 64 81)))
#;(check-eqv? (grid-map '() add1) '())
#;(check-eqv? (grid-map '((a b c) (d e f) (g h i)) (λ (s) (symbol=? s 'd)))
              (list (list false false false)
                    (list true false false)
                    (list false false false)))

;;grid-index-andmap : [Grid X] [Nat Nat -> Boolean] -> Boolean
;;Do all the indices in this grid pass the given predicate?
(define (grid-index-andmap g pred)
  (local [(define gh (length g))
          (define gw (if (empty? g) 0 (length (first g))))]
    (andmap (λ (ri) (andmap (λ (ci) (pred ri ci)) (build-list gw identity)))
            (build-list gh identity))))
#;(check-eqv? (grid-index-andmap '((a b c) (d e f) (g h i)) =) false)
#;(check-eqv? (grid-index-andmap '() >) true)

;;update-player-role : Player -> Player
;;Assign this player a new role at random
(define (update-player-role p)
  (make-player (random-boolean)
               (player-sender p)
               (player-receiver p)
               (player-score p)))
#;(check-eqv? (player? (update-player-role TESTPLAYER1)) true)
#;(check-eqv? (player-sender (update-player-role TESTPLAYER2))
              (player-sender TESTPLAYER2))
#;(check-eqv? (player-receiver (update-player-role TESTPLAYER3))
              (player-receiver TESTPLAYER3))
#;(check-eqv? (player-score (update-player-role TESTPLAYER4)) 0)


;;update-grid-roles : [Grid Player] -> [Grid Player]
;;Assigns each player a new role at random
(define (update-grid-roles g)
  (grid-map g update-player-role))
#;(check-eqv? (update-grid-roles '()) '())
#;(check-eqv? (length (update-grid-roles (world-grid TESTWORLD1))) 3)
#;(check-eqv? (grid-index-andmap
               (world-grid TESTWORLD1)
               (λ (ri ci) (same-strategies?
                           (get-grid-element (world-grid TESTWORLD1) ri ci)
                           (get-grid-element (update-grid-roles (world-grid TESTWORLD1)) ri ci))))
              true)

;;update-state : World -> World
;;Give the world a new random world state and each player a new random role
(define (update-state w)
  (make-world (random-boolean)
              (update-grid-roles (world-grid w))))
#;(check-eqv? (world? (update-state TESTWORLD1)) true)
#;(check-eqv? (boolean? (world-state (update-state TESTWORLD1))) true)
#;(check-eqv?
 (grid-index-andmap (world-grid TESTWORLD1)
                    (λ (ri ci) (same-strategies?
                                (get-grid-element (world-grid TESTWORLD1) ri ci)
                                (get-grid-element (world-grid (update-state TESTWORLD1)) ri ci))))
 true)
#|RUN WORLD - TICK|#
;;tick-world : World Number Number -> World
;;Update the world
(define (tick-world w r c)
  (cond [(or (string? w) (empty? (world-grid w))) w]
        [(stop? w) (final-strategy-text (world-grid w))]
        [else (update-state (update-strategies (update-scores w r c)))]))
#;(check-eqv? (world? (tick-world TESTWORLD1 2 1)) true)
#;(check-eqv? (boolean? (world-state (tick-world TESTWORLD1 2 1))) true)
#;(check-eqv? (world-grid (tick-world (make-world (random-boolean) '()) 0 0)) '())
#;(check-eqv? (map (λ (r) (map player-sender r)) (world-grid (tick-world TESTWORLD1 2 1)))
              (list (list (make-strat false true)
                          (make-strat false true)
                          (make-strat true true))
                    (list (make-strat true true)
                          (make-strat false true)
                          (make-strat true false))
                    (list (make-strat true false)
                          (make-strat true true)
                          (make-strat false true))))
#;(check-eqv? (map (λ (r) (map player-receiver r)) (world-grid (tick-world TESTWORLD1 2 1)))
              (list (list (make-strat true true)
                          (make-strat false false)
                          (make-strat true false))
                    (list (make-strat true true)
                          (make-strat true false)
                          (make-strat false true))
                    (list (make-strat true true)
                          (make-strat false false)
                          (make-strat true true))))

;;run-world : World Number Number -> World
;;Produces the final state of the world given the reward and cost
(define (run-world w r c)
  (if (string? w) w
      (run-world (tick-world w r c) r c)))

#|RUN|#
;;run : -> World
(define (run)
  (local [(define parsed (parse-args args))]
    (run-world (initial-world (first parsed) (second parsed)) (third parsed) (fourth parsed))))

(run)