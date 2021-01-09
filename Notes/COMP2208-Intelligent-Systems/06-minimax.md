# Minimax

Search examples so far were looking at single-player problems. Adversarial games
are different. In such games, the AI must keep track of what the opponent is up
to.

> **Minimax** is the traditional way of viewing adversarial problems. **Max**
> usually refers to the computer player that tries to maximise its advantage,
> while **Min** tries to prevent **Max** from doing so. To stop **Max** from
> winning, **Min** always selects moves that minimise **Max's** advantage.

## The minimax algorithm

1. Traverse the tree using DLS with some l as the depth limit;
2. Apply the heuristic to the leaf nodes;
3. Propagate heursitc values up the tree;
4. Choose the move with _guaranteed_ best move.

Properties of the minimax search:

1. It is **complete** if the search tree is _final_;
2. It is **optimal** against an _optimal_ opponent (otherwise, you could play
   "hope chess" -- select moves expecting that the opponent is going to make
   a mistake and grant you the advantage for free);
3. Its **time complexity** is _O(b<sup>m</sup>)_;
4. The **space complexity** is **linear** though _O(bm)_ due to DFS.

## Evaluation functions

- Typically a linear function in which coefficients are used to weight game
- features;
- It is _unlikely_ to be an optimal function;
- Evaluation functions can be as simple or as complex as you can imagine.

## The Horizon Effect

> In many cases, it is impossible to traverse the whole tree to find the optimal
> solution.

This leads to the **horizon effect**: some significant events might happen at
depth _l + 1_ but you've only looked at depth _l_... The _further_ we look
agead, the better decisions we make.

## Quiescent Search

It might so happen that your evaluation function oscilates like crazy as you are
traversing the tree down. On level _n_ it says _good_, _n + 1_: _terrible_, and
so on... In these cases, it's better to abandon such branch for a
**more certain one** which you know is good.

The **Quiescent Searc** however, suggests a different approach. You should
**go deeper** into the uncertain branches until you reach a more stable
position. Then, evaluate!

## Single Extension

In a case where **one move is clearly better**, why consider the rest of the
moves? We already know how it's gonna go down.
