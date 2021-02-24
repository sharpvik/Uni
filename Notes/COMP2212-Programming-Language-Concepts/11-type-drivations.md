# Type Derivations

In a strongly statically typed programming language, we must be able to check
type of an expression. How do we do that? We use recursive type analysis on the
AST!

Let's look at some expression like this:

```ocaml
let (x: Int) =
    if (y < 0) then 5 else 42
in
    x + 69
```

The AST:

```
let
--> x
--> if-then-else
    --> less-than
        --> y
        --> 0
    --> 5
    --> 42
```

Let's replace all atoms like numeral literals with their types:

```
let
--> x
--> if-then-else
    --> less-than
        --> Int
        --> Int
    --> Int
    --> Int
```

Now, from the knowledge of how operators work, for example the `<` is of form

```haskell
(<) :: Int -> Int -> Bool
```

... we can derive types, travelling up the tree and simplifying it.

```
let
--> x
--> if-then-else
    --> less-than (Bool)
    --> Int
    --> Int
```

```
let
--> x
--> if-then-else (Int)
```

```
let
--> x (Int)
```

And this way, we've checked that `x` is, indeed, an `Int`.

This method can be scaled to larger expressions.

## Shorcomings of the Static Typing

In a language like Python, it is completely OK to write something like this:

```python
x = 30
y = "hello" if x < 42 else 3.14
z = y + 5
```

It is a ternary operator that returns `"hello"` on `True` and `3.14` on `False`.
The type mismatch is apparent, but Python checks types at runtime, and thus, it
does not have a problem with this. The exception will be thrown on line 3, where
we are trying to add `"hello"` to `5`, but that's a different story.

However, with static typing, we can't predict at compile time whether `y` will
be assigned a `string` or a `number` type, and therefore, we cannot allow such
ternary operators -- both branches must return value of the same type!
