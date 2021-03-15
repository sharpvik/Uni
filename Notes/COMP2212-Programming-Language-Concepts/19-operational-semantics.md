# Operational Semantics

Operational semantics is more concerned with showing the state transitions.
There are two "flavours" of operational semantics: **big step** and
**small step**.

- **Big step**: the binary relation is between terms and values. It represents
  the values that a term can evaluate to.
  - We write _E &dArr; V_ to mean that program _E_ evaluates to value _V_.
  - Modelling the runtime environment (e.g. a heap) is quite straightforward in
    this approach by defining the relation between runtime states and values.
  - However, this approach still doesn't account for the effects of
    non-terminating programs (just like Denotational semantics).
- **Small step**: these are given by an inductive relation between terms
  representing runtime states of programs.
  - Runtime state includes the heap, the stack, **program counter**, etc...
  - We typically represent changing program counters as changing terms of a
    language.
  - For example, we write `E -> E'` for our reduction relation. This means that
    program state `E` evaluates in 'one' step of evaluation to program state
    `E'`.
  - We can evaluate program step-by-step and clearly see how it behaves.
  - In this model, non-termination is a _valuable activity_ rather than a
    failure to compute a value.

## Big Step Semantics for Toy

> Below, I use `||` symbol instead of the &dArr; for simplicity.

These are the terminals:

```
------      ------      ------------------------
n || n      b || b      \(x : T) E || \(x : T) E
```

Literals `n`, `m`, `n'` after the `||` are thought of as the mathematical
numbers.

```
E1 || n     E2 || m     n < m
-----------------------------
       E1 < E2 || true

E1 || n     E2 || m     not (n < m)
-----------------------------------
         E1 < E2 || false


E1 || n     E2 || m     n + m = n'
----------------------------------
         E1 + E2 || n'


  E1 || true     E2 || V
--------------------------
if E1 then E2 else E3 || V

  E1 || true     E3 || V
--------------------------
if E1 then E2 else E3 || V
```

Let's look at the `let` blocks:

```
   E1 || V E2[V/x] || V'
----------------------------
let (x : T) = E1 in E2 || V'
```

Here, `[V/x]` means that we want to substitue `V` in place of `x` within the
body of `E2`, evaluate `E2` and see what value it produces.

```
E1 || \(x : T) E1'      E2 || V2      E1'[V2/x] || V'
-----------------------------------------------------
                    E1 E2 || V'
```

## Small Step Semantics for Toy

> Program literals are not included in the small step semantics as there are no
> computation steps there.

```
    n < m           not (n < m)
-------------      -------------
n < m -> true      n < m -> true
```

This is all good and well **if** `n` and `m` are literals. But what happens if
some or both sides of this expression are themselves expressions?

```
    E -> E'               E1 -> E'
---------------     ------------------
n < E -> n < E'     E1 < E2 -> E' < E2
```

Now let's look at the `+`:

```
n + m = n'           E -> E'              E1 -> E'
-----------     ---------------     ------------------
n + m -> n'     n + E -> n + E'     E1 + E2 -> E' + E2
```

And the `if then else` statements:

```
-----------------------------       ------------------------------
if true then E2 else E3 -> E2       if false then E2 else E3 -> E3

                  E1 -> E'
----------------------------------------------
if E1 then E2 else E3 -> if E' then E2 else E3
```

Now, the `let` and lambdas:

```
--------------------------------
let (x : T) = V in E2 -> E2[V/x]

                   E1 -> E'
------------------------------------------------
let (x : T) = E1 in E2 -> let (x : T) = E' in E2

------------------------
\(x : T) E1 V -> E1[V/x]

            E2 -> E'                       E1 -> E'
--------------------------------        --------------
\(x : T) E1 E2 -> \(x : T) E1 E'        E1 E2 -> E' E2
```
