# Type Rules For Toy

Let's review the grammar of the Toy Language:

```
T, U ::= Int | Bool
E    ::= n | true | false | E < E | E + E | x
        | if E then E else E
        | let (x : T) = E in E
```

Now we can construct some type derivation rules for it. We will also name those
rules for convenience in the future derivation practices.

```

---------- TInt         ----------- TBool
|- n : Int              |- b : Bool



|- Eb : Bool    |- E1 : T       |- E2 : T
----------------------------------------- TIf
      |- if Eb then E1 else E2 : T
```

The `TIf` rule means that when we have an `if-then-else` expression with a
`Bool` guard _E<sub>b</sub>_, and two expressions _E<sub>1</sub>_ and
_E<sub>2</sub>_ of the same type `T`, then we can derive that the whole
`if-then-else` expression is of that same `T` type.

Sort of like this in Haskell:

```haskell
ifThenElse :: Bool -> t -> t -> t
```

The `T` type in this case is polymorphic, the specifics don't matter as long as
_E<sub>1</sub>_ and _E<sub>2</sub>_ are of the same type.

Now, what about the `let` expression? What's its type derivation rule? Well,
it's not so simple... Let's talk about **Type Environments** first.

## Type Environments

These are practically mappings of the form

```
x : Int, y : Bool, z : Int, ...
```

that can provide additional type context where it's needed.

## The Type Derivation Rule for `let`

We can write `Г |- E : T` to mean, in environment `Г`, expression `E` has type
`T`.

```
Г |- E1 : T     Г, x : T |- E2 : U
---------------------------------- TLet
Г |- let (x : T) = E1 in E2 : U
```

The `Г` environment holds type information about _x_ and so we can check
equality between the tpye of _E<sub>1</sub>_ and _x_. We also know, that this
`let` clause binds _x_ to type _T_ within _E<sub>2</sub>_, so we extend its type
environment to include that additional knowledge.

Type environments allow us to formalise the concept of scope. The _x_ is not in
scope within _E<sub>1</sub>_ and so it was not included into `Г`, however, it is
in scope for _E<sub>2</sub>_!

## Other Type Rules

### Comparison

```
Г |- E1 : Int       Г |- E2 : Int
--------------------------------- TLT
       Г |- E1 < E2 : Bool
```

### Addition

```
Г |- E1 : Int       Г |- E2 : Int
--------------------------------- TAdd
       Г |- E1 + E2 : Int
```

### Variables

```
x : T in Г
---------- TVar
Г |- x : T
```

The `T in Г` is more of a syntactic check on the type.

These were all the induction rules for Toy! By the induction principle, this
defines the relation for every derivation program of the language.
