# Type Checking

Recall that the typing relation `Г |- E : T` is defined as the **smallest**
relation which satisfies the set of rules. Logically, if a given program `E` is
in this relation with type `T`, then the only way it got into the relation was
by using one of the type derivation rules.

> But which one?

## Syntax-Directed Rules

A set of inference rules `S` defined over programs used to define an inductive
relation `R` is called **syntax directed** if, whenever a program (AST) `E`
holds in `R` then there is a unique rule in `S` that justifies this. Moreover,
this unique rule is determined by the syntactic operator at the root of `E`.

For example: in our Toy language, this term is well typed and has type `Int`.

```ocaml
let
    foo =
        \(x : Int)
            if x < 3 then 0 else x + 1
in
    foo 42
```

Note that the **last** rule used to derive this fact must have been `TLet`! We
know this just from looking at the syntax tree.

> In fact, for a syntax-directed set of type rules we see that the structure of
> type derivation trees matches the structure of the AST of the program that we
> are deriving a type for.

## Inversion Lemma

Another important property that we desire of a typing relation is that of
**inversion**. This refers to the ability to infer the types of subprograms from
the type of the whole program -- essentially by reading the type rules from
bottom to top.

Here is the Inversion Lemma for the Toy language:

- If `Г |- n : T` then `T` is `Int`
- If `Г |- b : T` then `T` is `Bool`
- If `Г |- x : T` then `T` is in the mapping `Г`
- If `Г |- E1 < E2 : T` then `Г |- E1 : Int` and `Г |- E2 : Int`
  and `T` is `Int`
- If `Г |- E1 + E2 : T` then `Г |- E1 : Int` and `Г |- E2 : Int`
  and `T` is `Int`
- If `Г |- if E1 then E2 else E3 : T` then `Г |- E1 : Bool` and `Г |- E2 : T`
  and `Г |- E3 : T`
- If `Г |- \(x : T) E : U` then `Г, x : T |- E : U` and `U'` is `T -> U`
- If `Г |- let (x : T) = E1 in E2 : U` then `Г, x : T |- E2 : U` and
  `Г |- E1 : T`
- If `Г |- E1 E2 : U` then `Г |- E1 : T -> U` and `Г |- E2 : T` for some `T`

From this lemma, we can immediately derive a type-checking algorithm as follows:

```haskell
typeCheck :: Term -> Env -> Maybe Type
typeCheck term env =
    case term of
        n -> Just Int
        b -> Just Bool
        x -> Just $ typeOf x env
        e1 < e2 ->
            if all (map (hasTpye Int) [e1, e2])
                then Just Bool else Nothing
        e1 + e2 ->
            if all (map (hasTpye Int) [e1, e2])
                then Just Int else Nothing
        if e1 then e2 else e3 ->
            if hasType Bool e1 && (typeCheck e2 env == typeCheck e3 env)
                then Just (typeCheck e2 env) else Nothing

        -- ... and so on
```
