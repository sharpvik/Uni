# Lexing Concepts

**Lexing** is the process of converting a source code string into a sequence of
_tokens_.

A **lexeme** is a pattern of characters in the input string that are considered
meaningful in the given programming language. These may be defined by regular
expressions as they are very simple.

A **token** is a lexeme that has been identified and "tagged" with a meaning and
possibly including a value. For example, the `while` lexeme can be transformed
into a token like `{type: Keyword, value: While}` and `42` is
`{type: Literal, value: 42}`.

## Scanning and Evaluation

### Maximal Munch Scanning

Lexemes are identified using a **scanner** that does that pattern-matching using
the _maximal munch_ approach which means that it takes the longest match. For
example, in a string `var a = 234345;`, when we find the first digit `2`, we
keep scanning all the other digits until the `;` is reached -- we don't stop
until we've exhausted all digits.

### Evaluator

Tokens are created using an **evaluator** whose job is to analuse the lexemes,
tag them appropriately and identify any associated value. For example, for a
pattern `[1-9]+` and the input `42`, the scanner would produce a lexeme `42` and
evaluator would identify it as `{type: Literal, value: 42}`.

## How To Write a Lexer?

Let's look at the general pipeline:

```
               *---------*                    *-----------*
--- String --> | Scanner | --- [ String ] --> | Evaluator | --- [ Token ] -->
               *---------*                    *-----------*
```

This code is _extremely_ generic given the following conditions are true:

- We know what lexemes look like (regexes)
- We know how to identify lexemes and associate values

If we have a means of describing these things then we could just automatically
generate the code to do the scanning and evaluation. This is great for code
re-use leading to productivity gains and robustness of code.

## Lexer Generators

A lexer generator is a software that takes in lexeme patterns and identifier
hints and generates a lexer for you.

> Such generators are widely available for free for most programming languages.

The majority of them are based around the C/Unix based tools called `lex` and
`flex`. There's `JLex` in Java and `ply.lex` in Python.

The input languages for these versions are mostly similar to `lex`, so learning
to use one version will make it easy to learn a version in another also. In the
future, we will be using the Haskell version of `lex` called `Alex` (haha).
