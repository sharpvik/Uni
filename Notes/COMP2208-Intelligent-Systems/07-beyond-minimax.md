# Beyond Minimax

There are so called **strong methods** to minimise the branching factor _b_.
These are based on the domain-specific knowledge about the game. For example, we
might notice that in many cases, in Noughts & Crosses, the moves lead to
symmetric positions on the board. We don't have to consider all rotations of
position _X_ really.

A **weak method** doesn't have any domain-specific knowledge, but its advantage
is that it can therefore by applied to any game (it's kinda polymorphic).

## Alpha-Beta Pruning

One of such weak methods is **Alpha-Beta Pruning**. It allows us to explore the
search tree using DFS but, at the same time, disregard certain branches of the
tree. This improves the search speed and feasible depth of search.

This algorithm uses two variables (_alpha and beta_). **Beta** represents the
upper bound on the node value (the _best_ we can do in this branch). It is
associated with the **Min** nodes and it _never increases_.

The **alpha** is the lower bound on the node value (the _worst_ we can do). As
opposed to _beta_, it is associated with the **Max** nodes and in
_never decreases_.

> If the _best_ we can do on the current branch is _less than or equal to_ the .
> _worst_ we can do elsewhere, there's no point continuing the evaluation on
> this branch.

## Directionality

Alpha-Beta pruning will remove different subtrees depending on the direction in
which the tree is traversed. Ordering has a large effect on the amount of
pruning!

> However, Alpha-Beta _is guaranteed_ to give the same values as Minimax.

If the tree is ordered s.t. _best moves examined first_, complexity drops from
_O(b<sup>d</sup>)_ (Minimax) to _O(b<sup>d/2</sup>)_.

> But the perfect ordering is not possible! If it was, we wouldn't need
> Alpha-Beta pruning. Yet, the runtime in practice is closer to the optimistic
> estimate, so we use it anyways.

## Alpha Go and Monte-Carlo Search

Alpha Go succeeded to beat the world champion in Go due to its better evaluation
function alongside with its use of Alpha-Beta pruning for Minimax.

Monte-Carlo Tree Search goes as follows:

1. Explore the tree up to depth _l_;
2. To evaluate the leaf nodes, we continue the game with random moves for both
   sides until it's over and someone wins or loses. This is a dynamic evaluation
   function.

Monte-Carlo search, even though it is noisy and random in theory, performs well
in practice due to the fact that it allows the algorithm to foresee the
significant moments that can tilt the game completely one way or the other.
