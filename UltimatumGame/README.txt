THE MINI ULTIMATUM GAME
In the ultimatum game two players are told they have 10 dollars to split. The players flip a coin to decide their role in the game - either the proposer or the responder. The proposer then makes an offer (e.g. "I will take 6 dollars, you take 4") and the responder can say yes or no. If the responder accepts the offer both players get what was agreed upon. If the responder rejects the offer both players get nothing.
There are infinite strategies to this game so I have made it into a simpler "mini" ultimatum game. The only available strategies for a proposer are to propose 1,2,3,4,5,6,7,8,9,or 10 dollars. The only available strategies for a responder are to accept a minimum of 1,2,3,4,5,6,7,8,9,or 10 dollars.

THE CODE
Each player has 2 strategies: a proposing strategy (represented by a number between 1 and 10) and a responding strategy (represented by another number between 1 and 10). Each strategy is represented by a color.
Red = 1, Orange = 2, Yellow = 3, Green = 4, Blue(bright) = 5, Indigo(dark) = 6, Purple = 7, Pink = 8, Brown = 9, Black = 10
In every step of the game each player is assigned a role at random, either proposer or responder. The players then find their average score against those neighbors which are playing the opposite role. All players then compare how they did to how their neighbors playing the same role did. If a certain neighbor did better then they will copy the strategy that player used (but only for the role they were playing). It is important to note that the grid of players wraps around, so players on the edge still have 8 neighbors to play.

TO RUN
If you have Racket installed on your computer you can run MiniUltimatumGame.rkt but if you don't you can still run the program using the executable file.
