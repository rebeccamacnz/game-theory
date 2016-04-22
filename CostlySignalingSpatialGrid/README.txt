COSTLY SIGNALING
A signaling game has two players – a sender and a receiver. The sender observes the state of the world and can then choose to send some signal to the receiver. The receiver then chooses an action to take. If this action is appropriate for the state of the world, both players receive some utility. Signaling games are one way in which we examine the evolution of language. Signaling games are usually visualized through a tree diagram such as NoCostSignalingTree.png
However, sometimes it is expensive to send a signal, such as in CostlySignalingTree.png

THE CODE
In CostlySignaling.rkt we explore what happens with a simple costly signaling game in the spatial grid. Each round there is a random state of the world and each player is assigned a random role (either a sender or a receiver). The players then interact with their eight neighbors and get some payoff. They then examine the payoffs of their neighbors, find the highest-scoring neighbor with their role, and update their strategy to match that player. Each player has two strategies, what to do as a sender and what to do as a receiver. The players are represented by a 2x2 grid of colors. The top left cell is their sender strategy in world state 1 (where red is send m1 and green is send m2), the top right cell is their sender strategy in world state 2, the bottom left cell is their receiver strategy when they see signal m1 (where green is do a1 and red is do a2), and the bottom right cell is their receiver strategy when they see signal m2. If vertical lines of red and green start to develop, this means that communication is evolving.

If you would like to run the program without images you can use CostlySignalingNoImages.rkt which is used for running the program in the command line. You will have to input 4 arguments: the grid width, grid height, reward, and cost. The reward used in CostlySignaling.rkt is 2 and the cost used is 1.

In CostlySignalingNoImages_UnevenStates.rkt we explore the game shown in CostlySignalingTree_UnevenStates.png (when the reward is 2 and the cost is 1). If you would like to explore other rewards and costs keep in mind that world state 1 will always give a reward 5 times greater than world state 2.

TO RUN
If you would like to run CostlySignaling.rkt and you have racket you can do so in Dr. Racket or by calling "racket CostlySignaling.rkt" in the command line. Alternatively, you can run the executable file in CostlySignaling.zip.

If you would like to run CostlySignalingNoImages.rkt you can do so in the command line by calling "racket CostlySignalingNoImages.rkt gw gh r c" where gw is the grid width, gh is the grid height, r is the reward for taking the correct action in the correct state, and c is the cost of signal m2.

If you would like to run CostlySignalingNoImages_UnevenStates.rkt you can do so in the command line by calling "racket CostlySignalingNoImages_UnevenStates.rkt gw gh r c" where gw is the grid width, gh is the grid height, r is the reward for taking the correct action in the correct state, and c is the cost of signal m2.