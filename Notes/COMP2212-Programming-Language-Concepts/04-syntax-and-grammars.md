# Syntax and Grammars                                                            

## Syntax vs Semantics

**Syntax** is a structure of statements in a program. It typically follows a
grammar based upin certain lexical symbols (e.g. keywords, control flow 
statements, etc.)

**Semantics** refer to the meaning of a program and how programs execute.

The role of an interpreter or compiler of a language is to transform syntax into
semantics.

## Grammars and Languages

Most programming languages have Context Free Grammar. Backus-Naur Form is the
_de facto_ standard for defining the grammar of a programming language.

BNF acts as a _metalanguage_ for defining languages. It is a convenient 
mets-syntax for defining the grammar of a language.

## Non-Terminals vs Terminals

**Non-terminals** represent different states of determining whether a string is
accepted by the grammar. In programming language terms these refer to the
different kinds of expressions one may have in the language.

**Terminals** represent actual symbols that appear in the strings accepted by the
grammar. The fancy ford is **tokens** or **lexemes**.

## Example BNF Grammar

In BNF, non-terminals are written in `<angular_brackets>` and the terminals are
written without.

```bnf
<program> ::= begin <stmt_list> end
<stmt_list> ::= <stmt> | <stmt>; <stmt_list>
<stmt> ::= skip | <assgn>
<assgn> ::= <var> = <expr>
<var> ::= X | Y | Z
<expr> ::= <var> + <var> | <var> - <var> | <var>
```

Example program accepted by this grammar:

```ada
begin
X = Y + Z;
skip;
Y = Z;
Z = Z - X
end
```

## Parse Tree

The legal programs of a language are those strings for which there is a 
derivation in the BNF grammar for that language. 

A **derivation** of a string in a BNF grammar can be represented as a tree. At
each node, the tree represents which rule of the grammar has been used to 
continue deriving the string. The childd nodes represent the matches of the 
substrings according to the grammar.

We call such trees **parse trees**.

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

## Syntax to Execution

We understand programs as strings of text, parsed as a tree according to some 
grammar, usually expressed in BNF. But how _exactly_ do we find the derivation of
a string in a grammar?

1. **Lexing** involves breaking the input stream of characters into _tokens_
   that are tuples of two values:
    - Token type (keyword, literal, operator)
    - Token value (`"if"`, `42`, `+`)
2. **Parsing** involves taking the tokens and forming a _concrete syntax tree_
   also referred to as the _parse tree_.
3. Concrete syntax trees contain semantically meaningless parts like grouping
   brackets and other syntactic conveniences. This is why we want to construct an
   **Abstract Syntax Tree (AST)**.
