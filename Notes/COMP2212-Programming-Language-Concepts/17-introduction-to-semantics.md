# Introduction to Semantics

**Semantics** refers to the _meaning_ of programs. It has to do with the
specification of a program's runtime behaviour. That is, what values it computes
and what side-effects it has.

The semantics of a _programming language_ is a specification of how each
language construct affects the behaviour of programs written in that language.

> Perhaps the most definitive semantics of any given programming language is
> simply its compiler or interpreter: if you want to know how a program behaves
> then just run it!

However, there are reasons we shouldn't be satisfied with this as a semantics
description.

## Need for Formal Semantics

Compilers and interpreters are not so easy to use for reasoning about behaviour.
This is because not all compilers are the same (e.g. `clang` vs `gcc` and also
each has different versions following different C standards).

Compilers and interpreters are also quite large in size and so it is common that
they contain bugs that deviate from the intended behaviour.

For compilers, the produced low-level code is often unreadable. It is hard to
use compiler source code to trace the souce of subtle bugs in your code due to
strange interpretations of language operators. Moreover, compilers opimise your
programs (allegedly in semantically safe ways) for maximum efficiency. This can
disturb the structure of your code and make reasoning about it much harder.

As you can see, the real job of a compiler is not to explain the behaviour of a
program but to make it run on a machine.

## Advantages of Formal Semantics

In contrast, a formal semantics system should be precise (like a compiler) but
written in a formalism more amenable to analysis.

- Written in some form of logic or some other mathematical language.
- With no worries about efficiency or execution speed, we can focus on
  unambiguous specification of the meaning of the language constructs.
- Semantics can profide us with a proper reference for a language s.t. any valid
  compiler should produce results that match that reference.
- The formal semantics can be built in compositional ways that reflect
  high-level program structure.

## Approaches to Semantics

- **Denotational Semantics** advocates mapping every program to some point in a
  mathematical structure that represents the values that the program calculates.
  - `if 0 < 1 then 0 else 1 => 0` is an example of such mapping.
- **Operational Semantics** uses relational approaches to specify the behaviour
  of programs directly. Typically inductively defined relations between programs
  and values they produce, or states the programs can transition between are
  used.
  - `if 0 < 1 then 0 else 1 => if true then 0 else 1 => 0` describes states.
- **Axiomatic Semantics** take the approach that the meaning of a program is
  just what properties you can prove of it using a formal logic.
  - e.g. Hoare Logic
