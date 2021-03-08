# Denotational Semantics

To give a denotational semantics one must first identify the **semantic domain**
into which we will map programs.

Elements in the semantic domain represent the _meanings_ of programs. For
example, for programs that return a positive integer, a reasonable choice of
semantic domain is the natural numbers.

## Denotational Semantics for the Toy Language

First, we choose the semantic domains. These will be **N** and a different two
element set **B** = {`true`, `false`}. We will also make use of the function
spaces between these sets.

Let's define [[T]] do be **N** when T is `Int` and **B** when T is `Bool` and
define [[T &rarr; U]] = [[T]] &rarr; [[U]].

Our aim now is to provide a function [[-]] from well-typed programs E of type T
to the semantic domain [[T]], that is

Given `|- E : T`, then [[E]] should be a value in [[T]].

## Interpreting Type Environments

In trying to interpret functions we will need to interpret function bodies.

- These may contain free variables
- We will need to interpret terms with possibly free variables in them.

We need to have an environment to provide values for the variables.

Given a term `Г |- E : T` then we need an interpretation [[E]] that makes use of
an environment `e` that maps each free variable in `Г` to a value in the
semantic domain. We write [[E]]<sub>e</sub> to denote this.

We say that `e` _satisfies_ `Г`, written as `e |= Г`, if
`Г(x) = T => e(x) in [[T]]`.

We require the property that, for `Г |- E : T` and for all `e` s.t. `e |= Г`,
[[E]]<sub>e</sub> : [[T]].

## Defining the Denotational Function for Toy

```
            Toy             |            Maths
----------------------------------------------------------
                 [[true]] e = true
                [[false]] e = false
                    [[n]] e = n
                    [[x]] e = v
               [[E < E']] e = true if [[E]] e < [[E']] e
               [[E < E']] e = false    otherwise
               [[E + E']] e = [[E]] e + [[E']] e
[[if E then E' else E'']] e = [[E']] e if [[E]] e = true
[[if E then E' else E'']] e = [[E'']] e if [[E]] e = false
           [[\(x : T) E]] e = v -> [[E]] e [x -> v]
[[let (x : T) = E in E']] e = [[E']] e [x -> [[E]] e]
```

Where `e [x -> v]` means update the mapping `e` with a map from `x` to value
`v`.

## Comments and Criticisms

A criticism one might have of denotational semantics at this point is that they
don't give a clear account of **how** the program is actually supposed to
execute. What are the state transitions?

Instead, they give a very precise and nicely compositional account of what
values the program is supposed to calculate. This abstract away all of the
execution steps.

This can be useful for modelling pure functional languages, but is not a good
match for procedural languages with mutation, loops, and so on everywhere.

Modelling recursion denotationally can also be challenging since it is a loop
(that might not even terminate). So what value do we map it to?

A _major_ criticism of the above denotational model of the Toy language is that
there is a lot of "junk" in the model. For example, the semantic domain
[[Int -> Int]] is a set of **all** functions from **N** to **N** which includes
uncomputable functions. This set is also uncountably infine... Such model is too
big in a sense.

There is lots of research into finding denotational models that are "just right"
for a language. This is a difficult in general, even for small toy languages.
