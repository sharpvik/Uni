# Ambiguous Grammars

Let's look back at the example from the last lecture:

## Parse Tree Example

Grammar:

```bnf
<expr> ::= <expr> + <expr> | <expr> * <expr> | <lit>
<lit> ::= 1 | 2 | 3 ...
```

For the following expression:

```
2 + 3 * 4
```

We can construct the following tree:

```
<expr>
--> <expr> + <expr>
    --> <lit> --> 2
    --> <expr> * <expr>
        --> <lit> --> 3
        --> <lit> --> 4
```

You might have noticed that this is not the only way to construct a parse tree
for the given expression. We can also do this:

```
<expr>
--> <expr> * <expr>
    --> <expr> + <expr>
        --> <lit> --> 2
        --> <lit> --> 3
    --> <lit> --> 4
```

## Ambiguous Grammars

We say that grammar _G_ is **ambiguous** if there exists a string _s_ for which
there exist two or more different parse trees using the rules of _G_.

> Grammar ambiguity is generally considered to be a bad thing as we want
> programs to be deterministic, not ambiguous!

## Resolving Ambiguous Grammars

We can, of course, use parenteses everywhere to make sure the compiler knows
what we really mean, but this doesn't look too appealing as it impacts
readability.

Instead, we can use **operator precedence** rules to say that the `*` operator
has higher precedence than the `+` operator, and so the correct parse tree can
be constructed without the use of brackets.

## Resolving Ambiguous Grammars Example

Let's look at how we can construct an unambiguous grammar:

```bnf
<expr> ::= <mexpr> + <expr> | <mexpr>
<mexpr> ::= <bexpr> * <mexpr> | <bexpr>
<bexpr> ::= (<expr>) | <lit>
<lit> ::= 1 | 2 | 3 ...
```

Notice how the _level_ of non-terminals determines the precedence of operations.
Brackets are used to "reset" that precedence.

Let's see the parse tree derivation from `2 + 3 * 4`:

```
<expr>
--> <mexpr> + <expr>
    --> <bexpr> --> <lit> --> 2
    --> <mexpr>
        --> <bexpr> * <mexpr>
            --> <lit> --> 3
            --> <bexpr> --> <lit> --> 4
```

## Associativity

Let's look at the following string: `2 + 3 + 4`. If we were to construct a parse
tree using the updated grammar, is it still unambiguous? It is, but it is
right-associative -- in other words, the string will be parsed as `2 + (3 + 4)`.

In a language that only supports `+` and `*`, this isn't a problem since
`2 + (3 + 4)` is the same as `(2 + 3) + 4`. Both `+` and `*` are associative in
maths. However, if we were to support operators like `-` and `/`, we'd have to
update our grammar again because `2 - (3 - 4)` is not the same as `(2 - 3) - 4`.

To change associativity, we can rewrite the grammar, swapping places as follows:

```bnf
<expr> ::= <expr> + <mexpr> | <mexpr>
<mexpr> ::= <mexpr> * <bexpr> | <bexpr>
<bexpr> ::= (<expr>) | <lit>
<lit> ::= 1 | 2 | 3 ...
```

> Be **really careful** with this approach. This grammar is now left-recursive
> and this won't work well with recursive descent parsing! You might get stuck
> in a loop.

## The Dangling `else` Problem

In many programming languages, we can write an `if-then` statement without an
`else` branch. Here's an example grammar:

```bnf
<ifstmt> ::= if <expr> then <stmt> else <stmt>
           | if <expr> then <stmt>
```

> This grammar is ambiguous :(

Let's look at a program:

```bash
if true
    then if false
        then skip
else loop
```

Which `if` does the `else` branch correspond to..?

## Solution to the Dangling `else`

You can find a solution in the Java grammar. They use additional non-terminals
to determine a precedence that a nested conditional in a `then` branch cannot
use a single branch conditional.

```bnf
<if-then-stmt> ::= if ( <expr> ) <stmt>
<if-then-else-stmt> ::= if ( <expr> ) <stmt-no-short-if> else <stmt>
<if-then-else-stmt-no-short-if> ::= if ( <expr> ) <stmt-no-short-if> else
                                    <smtm-no-short-if>
```
