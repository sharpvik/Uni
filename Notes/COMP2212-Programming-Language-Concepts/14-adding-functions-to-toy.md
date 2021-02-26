# Adding Functions to Toy

We are goin to try and think about how to add some lambda calculus kind of
functions to our Toy language.

## Simply-Typed Lambda Calculus

We can formalise a simple type system for labmda calculus. Without some base
types though, this is a very boring language, so let's look at it combined with
the Toy language. We'll call the resulting language Lambda-Toy.

Here's the revised grammar:

```
T, U ::= Int | Bool | T -> T
E ::= n | true | false | E < E | E + E | x
     | if E then E else E
     | \(x : T) E
     | let (x : T) = E in E
     | E E
```

The type rule for a lambda:

```
    Г, x : T |- E : U
------------------------ TLam
Г |- \(x : T) E : T -> U


Г |- E1 : T -> U        Г |- E2 : T
----------------------------------- TApp
          Г |- E1 E2 : U
```

## Types in Lambda Calculus

```
\(x : T) x : T -> T                             -- identity function

\(x : T) \(y : U) x : T -> U -> T               -- first projection

\(x : T) \(y : U) y : T -> U -> U               -- second projection

\(f : T -> T) \(x : T) f (f x)                  -- double application
    : (T -> T) -> T -> T

\(g : T -> U) \(f : U -> V) \(x : T) f (g x)    -- composition
    : (T -> U) -> (U -> V) -> T -> V
```

Let's see an example of type derivation for one of the expressions above:

```
       y : U in { x : T, y : U }
       ------------------------- TVar
        x : T, y : U |- y : U
    ---------------------------- TLam
    x : T |- \(y : U) y : U -> U
------------------------------------ TLam
|- \(x : T) \(y : U) y : T -> U -> U
```
