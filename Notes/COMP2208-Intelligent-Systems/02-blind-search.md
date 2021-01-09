# Blind Search

## Problem-Solving Agents

> Goals help organise behaviour by limiting the objectives that the agent is
> trying to achieve and hence the actions it needs to consider.
> **Goal formulation**, based on the current situation and the agent's
> performance measure, is the first step in problem solving.

Goal is a **set of world states** in which the desired outcome is reached.

> **Problem formulation** is the process of deciding what actions and states to
> consider, given a goal.

If the agent has no idea about the changes to its environment that a certain
action is going to bring about (the environment is **unknown**), it has no
choice but to try one action _at random_ and see what it does.

Otherwise, the agent would be able to consider _subsequent_ stages of a
hyptothetical journey and decide which path is better. This is a search problem.

> An agent with several immediate options of unknown value can decide what to do
> by first examining future actions that eventually lead to states of known
> value.

In some cases, solution may not be described as a well-defined path, but rather
as a list of suggestions based on the possible percepts that may arrive in the
future.

If problem environment is **deterministic and fully observable**, then agent
knows exactly what each action does and solution is a sequence of actions.

## Well-Defined Problems And Solutions

A **problem** can be formally defined by the following components:

1. The **initial state** that the agent starts in;
2. A description of the possible **applicable actions** available to the agent;
3. A description of what each action does (**transition model**);
4. The **goal test** which determines whether a given state is a goal state;
5. A **path cost** function that assigns a numeric cost to each path.

> A **solution** to the stated problem is an **action sequence** that leads from
> the initial state to the goal state.

> Solution quality is measured by the path cost function. An
> **optimal solution** has the lowest path cost among _all_ solutions.

## Measuring Problem-Solving Performance

1. **Completeness**: will we _always_ find a solution _whenever there is one_?
2. **Optimality**: will we find the _best possible_ solution?
3. **Time complexity**: how long is it going to take to find a solution?
4. **Space complexity**: how much memory is needed to perform the search?

Time and space complexity are measured in terms of:

- _b_: maximum branching factor of the search tree;
- _d_: depth of least-cost solution;
- _m_: maximum depth of the state space (may be infinite).

## Uninformed Search Strategies

### Breadth-First Search

Nodes are expanded starting with the root node **level after level**. It is
usually implemented using the queue data type.

> BFS is **complete**, **optimal**, however its space and time complexity is not
> so good. The **space complexity** is O(b<sup>d</sup>) where both expanded and
> visited nodes are stored in memory. The time complexity is a lesser concern,
> however, still a big one as all those nodes have to be generated.

### Uniform-Cost Search

UCS is a modification on BFS that allows us to find least cost solution in cases
where the step costs are unequal. We simply expand the least cost path first,
storing nodes in a priority queue ordered by said cost function's value.

### Depth-First Search

Nodes are expanded starting with the root node **in a penetrative style**. It is
usually implemented using the stack data type.

> DFS is **complete** in finite state spaces because it will eventually expand
> every node. It is **not optimal** as it does not expand frontier in the
> lest-cost fashion, unlike BFS. Its space complexity, however, is much better
> if compared to the BFS since we can deallocate memory associated with the
> fully expanded branches of the tree.

Backtracking search (a modification upon DFS) takes up even less memory: only
O(m) instead of O(bm). It can do so due to the fact that we can generate only
one successor when expanding a node, instead of generating all of its children.

### Depth-Limited Search

To overcome DFS complications with infinite state spaces, we can supply to it
some depth limit _l_ such that nodes at that level are treated as leafs.
However, this might introduce more incompleteness if we choose _l < d_.

### Iterative-Deepening Depth-First Search

To combine benefits of the BFS and DLS, we can use the Iterative-Deepening
strategy. We will apply DLS to the given tree, gradually increasing depth limit
_l_ up to _d_ until a solution is found.

> This algorithm has a **modest space and time complexity** while also keeping
> BFS **optimality and completeness** guarantees.

Here's the pseudocode:

```python
def ids(tree: Tree) -> Maybe Solution:
    for depth in [0..]:
        result: Maybe Solution = dls(tree, depth)
        if result != Nothing: return result
```

### Bidirectional Search

We can run two simultaneous searches starting from the start and the goal state,
trying to meet in the middle. This is called **bidirectional search**.

> This is desirable since _b<sup>d/2</sup> + b<sup>d/2</sup> < b<sup>d</sup>_.

To implement this search strategy, at each step we must check whether the node
is in the fringe of the other search before we expand it. If yes, a solution is
found!

> It is important to remember that this search strategy assumes that the
> predecessor `pred(state)` function is easily computable. Sometimes, that might
> not be the case: for example, in chess there are multiple board arrangements
> where the King is checkmated, and each of those have multiple possible
> predecessor states...
