# Local Search

> We use **local search** when the goal state is not clearly defined as in the
> case of Romanian map example.

In chess, for example, the goal state is defined as _a board configuration such
that the king is attacked and cannot get out of that attack through any legal
means available_. This definition does not specify a certain concrete
implementation, but rather, a whole set of possible goal states.

## Hill Climbing Search

In cases like this, we can use the **hill climbing search** algorithm to find a
solution.

The hill climbing search is called a local search because it does not have any
long-term memory, so to speak. It is also not optimal, as it halts whenever a
maximum (peak) found, since it has no way to predict whether there exists a
_higher maximum_ out there in the search space.

We can also thing about the hill climbind search as _minimising error_ where we
halt as soon as every legal next step increases the amount of error. Error
being, of course, the amount of unwanted properties in the environment.

This sort of _greedy_ nature of the algorithm is precisely what prevents it from
being **optimal**. In a configuration of the 8-queens problem where all eight
queens are in their own row and column and the number of interfering queens is
2 (there are two queens on the same diagonal), it is _impossible_ to improve the
position and reach a goal state. Our algorithm will not be willing to
temporarily sacrifice correctes in order to reach the goal state in the future.

## Simulated Annealing Search

> The general idea is to allow the algorithm to make moves that might increase
> the error in an attempt to reach a higher maximum.

To allow for such mechanism, while decreasing the probability of it missing the
global optima, we use _temperature_. The higher the temperature, the crazier we
go, allowing all sorts of unoptimal transgressions. However, with time, system
_cools down_, decreasing the probability of down-slope moves.

In general, down-slope moves occur with a probability that depends on the
temperature and the difference between the current node and the node being
evaluated.

```python
import math

while move:  # at each move
    # error difference between the current and potential states
    dE = E(next) - E(current)
    if dE > 0:  # if that move is an improvement ...
        current = next  # move there
    else:  # next increases error
        move_probability = math.e ** (dE / T)
        current = next if move_probability > threshold else current
```

In terms of optimality, simmulated annealing is _not 100% optimal_, but it was
proven that if you decrease the temperature _slowly enough_, the probability of
finding the global maximum approaches 1, which means that this search is
**very close to optimal**.

> However, it must be noted that that proof relies on the
> **extremely small temperature decrease**. In fact, you can make it so slow
> that the search simply explores the whole search space (which is not what you
> really want from a search algorithm -- it's not efficient). If you had
> _enough time to explore the entire search space_ you might have just stopped
> at the global maximum straight on...

In practice, however, annealing with a _reasonable_ tempreature setting works
quite well and is widely used in airline scheduling, neural nets, and more.

## Local Beam Search

> Here, instead of conducting a _strict local search_, we keep track of _k_
> steps. If any of those states is a goal state, just stop there, otherwise,
> pick _k_ best states out of the ones already explored and expand them.

**Local beam seach** exploits parallelism and is able to jump between branches
of expanded nodes if necessary.

Nevertheless, beam search can still get stuck in a narrowly focused region of
the search space.

## Genetic Algorithms

We start with a population of size _k_ of randomly generated states (agents can
represent states of our search for example if we are searching for the optimal
way to play flappybird).

A state is represented by a final string over a final alphabet.

We use **fitness function** to decide whether a change is good or bad. In
case of the flappybird, the game score might serv as fitness function.

Then we produce the next generation of states by
**selection, mutation, and crossover**.

## Online Search

In this scenario, an agent has no idea about the outcome of their actions. In
other words, the only way to generate a node is to commit to a step. It only
works if state space is **spacely explorable**.

> This is not a very pleasant situation to be in. The only thing we can really
> do here are **random walks**. Hopefully, the state space is reasonably small,
> because this gets exponentially harder and takes a lot of space.

This is not really clever...
