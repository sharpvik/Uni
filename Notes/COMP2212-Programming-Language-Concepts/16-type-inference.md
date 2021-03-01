# Type Inference

You will have noticed that in our Toy language we explicitly declared the type
of arguments to functions and local variables like so:

```ocaml
\(x : T) E

let (x : T) = E1 in E2
```

This is commonplace in many mainstream programming languages - especially
statically typed ones like C, Java, Rust, etc. Programmers coming from a
dynamically typed languages often criticise that, saying that explicitly
declaring types is a burden on a programmer!

It seems like if would be better to have compiler be able to statically derive
types and check them. This way, if you don't want to declare types, you don't
have to!

For example, in expression like `let x = 20 in y + x` our compiler would be able
to reasonably infer that `x, y : Int`.

## Type Inference

Type Inference is algorithmically more complicated than Type Checking.

Let's look at the rule for lambda in our Toy language to see why.

```
    Г, x : T |- E : U
------------------------ TLam
Г |- \(x : T) E : T -> U
```

Now, suppose that we don't know `T` and `U` as part of the syntactic definition.
Then, the rule looks like this:

```
 Г, x : ? |- E : U
-------------------- TLam
Г |- \(x) E : ? -> U
```

Not so clear what to do now...

## Type Variables and Unification

The approach often taken to solve this problem is to introduce _Type Variables_.
These are symbolic values that represent an unconstrained type. When typing a
function with unknown types, these variables are used and type checking
continues.

As part of checking, certain _constraints_ on these type variables will arise.
E.g. if an argument to a function of unknown type is used as a guard of an `if`.
Then, all of a sudden, we know that it's a `Bool`. This way, the type-checking
algorithm will produce, for each well-typed program, a type that may contain
variables, along with a collection of constraints of these type variables.

To obtain an actual type of the program we need to solve the constraints. We
find a substitution of type variables such that all of the constraints hold.
This process is called **unification**.

## Example of Unification

Let's infer types for this Toy program with implicit types:

```ocaml
let foo = \x if x < 3 then 0 else x + 1
in let cast = \y if y then 1 else 0
in cast (foo 42)
```

1. Unfold the first `let`:
   `foo : a` and therefore, `\x : if x < 3 then 0 else x + 1 : a`.
2. Unfold the `\x` expression:
   constraint `a = c -> d` and
   `if x < 3 then 0 else x + 1 : d` assuming `x : c`.
3. `x < 3 : Bool`, `0 : Int`, `x + 1 : Int`, therefore, `x : c` and `c = Int`!
   We also know that `d = Int`.
4. Unfold the second let statement:
   `cast : e`, therefore, `\y if y then 1 else 0 : f` and `cast (foo 42) : f`.
   From this, `b = f`.
5. Unfold the `\y`: `e = g -> h` since it's a lambda,
   and `if y then 1 else 0 : h` assuming `y : g`.
6. From there, we can unfold the `if`: `y : Bool`, so `g = Bool`
   and `1, 0 : Int`, so `h = Int`.
7. Unfold the final applications:
   `cast (foo 42) : f` with the constraint `f = h` and further, by unfolding
   `foo 42` we get `c = Int`.

The problem here is that

```
d = Int
g = Bool
g = d ???
```

There is no substitution that makes this well-typed, which tells us that this
expression is ill-typed.
