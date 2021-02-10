# Parsing Concepts

We already know that **parsing** is the process of converting a stream of  
lexical tokens in to an AST by matching it against the grammar for the language.
Parsing is done after lexical analysis in the compiler/interpreter pipeline.

There are many different approaches to parsing; two most common are:

1. **Top-down** also called **recursive descent**
   - Builds parse tree _from root down_
   - LL grammars (reads left-to-right, forms leftmost derivations)
2. **Bottom-up**
   - Builds parse tree from leaves up
   - LR parsers (reads left-to-right, formst rightmost derivations)
   - More compilcated but handles a wider range of grammars as comared to LL

## Shift and Reduce

In the bottom-up approach, there are two very important terms: **shift** and
**reduce**. These are two main actions you can take after consuming a token.

- **Shift**: read the next input token and create a new subtree
- **Reduce**: apply a spotted grammar rule to join subtrees together

This process continues until an error occurs or EOF.

Sometimes, if your algorithm is non-deterministic, you get **conflicts** where
two or more actions can apply to a particular token you've just consumed.

## How to Write a Parser

Just like with lexers, we can _generate_ parser automatically, however, this
might not be the best thing to do, and quite often, it isn't.

Let's look at the parsing pipeline:

```
                       *----------------------------------*
                       |                                  |
                       ▼                                  |
                  *---------*                        *---------*
--- [ Token ] --> | Scanner | --- ( Next, Rest ) --> | Matcher | --- AST -->
                  *---------*                        *---------*
```

## Parser Generators

The most famous ones are:

- `yacc` (works with `lex`)
- Bison (works with `flex`)
- ANTLR4 is a more modern lexer and parser generator (adaptive LL parsers)
- JavaCC is a Java-based lexer/parser generator that uses the LL(k) algorithm

Both Bison and `yacc` produce LALR (Lookahead LR) parsers.
