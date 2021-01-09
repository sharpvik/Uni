# Constraint Satisfaction Problems

In a standard state space search problem, the state is a **black box** that
supports the successor function and a goal test.

In **Constraint Satisfaction Problems** we have access to the state. This allows
us to utilise heuristics and develop more powerful algorithms. To represent the
state we use variables _V<sub>1</sub> ... V<sub>n</sub>_ that can take values
from a domain of values _D_. There are **constraints** about variables that must
be satisfied in order to reach the goal state.

> The goal state is only ever reaches if all values have been assigned a value.

## Finite Domains

For _n_ variables, the domain size _d = O(d<sup>n</sup>)_. Boolean
satisfiability is included in the Constraint Satisfaction Problem (NP-complete).

## Infinite Domains

Strings, or integers. Job scheduling is one of such problems; where jobs can run
indefinitely.

> This requires a special representation language.

```json5
{
    "jobs":     ["A", "B", "C"],
    "machines": ["M", "N", "P"],
    "schedule": [
        {   // Initial configuration.
            "M": ["A", "B"],
            "N": [],
            "P": ["C"]
        },
        {
            /* Configuration after one hour. */
        },
        ...
    ]
}
```

## Variable Constraints

CSP problems vary in terms of how many variables are involved in the constraint.
For example the state colouring problem (where no two adjacent states can have
the same colour) has a **binary constraint** where _two_ variables are involved.

There are also **unary** and **higher-order** constraints.

## Real World Use Cases

CSPs are used for sheduling and timetabling.

## Formal Definition

Initially, all variables are `null`. **Successor function** assigns a variable
in a way that doesn't break constraints. It fails if there are no legal
assignments. With this configuration, it is enough to check that
_all variables have a value_ to know that we've reached the **goal state**.

> Every solution appears at depth _n_ where _n_ is the number of variables.
> Knowing this, we can use a simple Depth First Search.

The path is irrelevant here, we don't have to provide the sequence of steps as
part of the solution.

> _#leaves = n! d<sup>n</sup>_

## Backtracking Search

One of the ways to search CSPs is the **Backtracking Search** where we are
descending down a branch until we reach the goal state at depth _n_ or run into
a failure where no legal assigment is possible; in the latter case we simply
return and the search continues from the last "check point" LOL.

> Backtracking assigns **one variable at a time**.

1. To **improve backtracking efficiency**, it makes sense to select variables
   that share constraints.
2. _Picking the most constrained variables so far_ is another reasonable
   heuristic.
3. We can pick the **most constrain*ing* variable** if there are multiple
   variables with the same abount of remaining values and you need to break that
   tie.
4. While assigning the value to a variable, choose a value that is
   _least constraining to its neighbours_.

> Combining the above mentioned heuristics to the Backtracking Search increases
> its maximum feasible _n_ for the n-queens problems _from around 25 to 1000_.

Use **forward checking** to stop descent as soon as some variable
_has no more legal ramaining values_.

**Arc Consistency** is a form of propagation that re-checks neighbouring
variables if variable _x_ looses value. Basically, a form of inductive thinking.
Think of updating all affected cells after inserting a number into a sudoku
puzzle. As soon as we put 3 into a cell, all cells in that row, column, and
quadrant loose 3 as their possible value.

**Backtracking Search** is optimal! It only prunes out branches that lead to the
_inevitable failure_.

## Local Search For CSPs

Here, LS algorithms like hill-climbing usually work with all variables already
assigned (randomly), simply changing their values until it all "clicks" into
place.

> Variables to modify can be selected either at random (stupid) or if they
> are involved in a conflict (smarter approach).
