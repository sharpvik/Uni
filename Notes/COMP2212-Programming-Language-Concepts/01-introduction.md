# Introduction

## Major Programming Language Families

### Imperative Languages

- C/C++
- C#
- Java
- Pascal
- Rust
- Go
- Swift

These are called **imperative** because they, in a way, specify exactly how to
manipulate data in memory in order to achieve the desired result. For example:

```c
#include <stdio.h>

int main(void)
{
    int i = 42;     // Assign 42 to i
    while (i > 0)   // Loop until i = 0
        printf("I love number %d\n", i--);
}
```

These C instructions describe exactly what to put where and how to proceed then.
They are quite close to the machine code. In fact, if you _really_ try, you can
almost imagine the kind of assembly it produces. You can't do that with Java,
but the principle of commanding the hardware directly is still there.

### Declarative Languages

These are languages where instead of telling the machine _what_ to do and _how_
to do it, we specify _what we want_ to get as an output.

#### Functional Programming

- OCaml
- Haskell
- ML
- Lisp
- Scheme
- F#
- Closure

#### Logic Programming

- Prolog (and its variants)

#### Domain Specific

These languages allow non-professionals to write code that is closer to their
domain which they are more likely to understand.

- HTML
- Ant
- SQL
- XSLT
- SOAP

## Code Example (Merge Sort)

### Imperative Language

```pascal
const INFTY = maxint;
var a, b, c : array [1..10000] of integer;

procedure merge (l, m, r : integer);
var i, j, k : integer;
begin
    for i := l to m do b[i] := a[i];
    i := 1; b[m+1] := INFTY;
    for j := m+1 to r do c[j] := a[j];
    j := m+1; c[r+1] := INFTY; k := l;
    while (b[i] < INFTY) or (c[j] < INFTY) do
        if b[i] < c[j] then
            begin a[k] := b[i]; inc (i); inc(k); end
        else
            begin a[k] := c[j]; inc (j); inc(k); end;
end;

procedure mergesort (l, r : integer);
var m : integer;
begin
    if l < r then begin
        m := (l+r) div 2;
        mergesort (l, m);
        mergesort (m+1, r);
        merge (l, m, r);
    end;
end;
```

Here, you really have to think about the loops, indeces, increments and the
like...

### OCaml

```ocaml
open Future;;
open List;;

(* sort a list of integers l with parallelism *)
let rec mergesort (l: int list): int list =
    (* merge two sorted lists recursively *)
    let rec merge (l1, l2) =
        match (l1, l2) with ([], []) -> []
            | (l, []) -> l
            | ([], l) -> l
            | (hd1::tl1, h2::tl2) ->
                if (hd1 < hd2) then hd1::merge (tl1, l2)
                else hd2::merge (l1, tl2)

    in
    (* split a list in half *)
    let split l: int list * int list =
        match l with [] -> ([], [])
            | hd::[] -> (l, [])
            | _ -> let (l1, l2, _) = List.fold_left
                (fun (l1, l2, n) nxt ->
                    if (n > 0) then (nxt::l1, l2, n-1)
                    else (l1, nxt::l2, n-1)) ([], [], (List.length l)/2) l
            in (List.rev l1, List.rev 12)
    in
    (* perform mergesort with multiple threads *)
    match l with [] -> []
        | hd::[] -> [hd]
        | _ -> let (l1, l2) = split l in
            let f = Future.future mergesort l1 in
            let l2' = margesort l2 in
            let l1' = Future.force f in
            merge (l1', l2')
```

### Prolog

```prolog
mergesort([], []).      /* covers special case */
mergesort([A], [A]).    /* sorted singleton is itself */
mergesort([A,B|R], S) :-
    split([A,B|R], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).

split([], [], []).
split([A], [A], []).
split([A,B|R], [A|Ra], [B|Rb]) :- split(R, Ra, Rb).

merge(A, [], A).
merge([], B, B).
merge([A|Ra], [B|Rb], [A|M]) :- A =< B, merge(Ra, [B|Rb], M).
merge([A|Ra], [B|Rb], [B|M]) :- A > B, merge([A|Ra], Rb, M).
```

## Software Design Methodologies

### Data-Oriented

Data here is the thing you are manipulating. This makes you think about Abstract
Data Types and Object-Oriented Programming with data encapsulation and
inheritance.

### Procedure-Oriented

This emphasises decomposing code into logically independent _actions_. This is
often very good for concurrent programming!

### Cross-Fertilisation

A lot of functional programming concepts are used in imperative languages like
Java, C#, and Rust (especially Rust!).

And the other way around! In Haskell, monads allow functional programmers to
write imperative code.

Declarative code (e.g. SQL, HTML) very often comes intermingled with ordinary
code. Think about that time you communicated with the SQLite database from a
Python program.

### Evaluation Criteria

Syntax is very important for the ease of use, ease of committing it to memory,
readability, etc.

It is very important that the features of the programming language combine
together to produce readable and clear code; quite often, _code author_ and
_code maintainer_ are **different people**.

The syntax is supposed to be easy to memorise and understand. There is usually a
drawback between incorporating familiar elements from popular programming
languages and inventing a more domain-specific syntax which is more convenient
for your language's purpose.

Good programming languages _help programmers_ avoid bugs and safety issues by
providing quality tools like linters, code checkers, and an intelligent compiler
that warns you of common mistakes.

Another important detail: we should provide an _appropriate abstraction level_.
You need to decide whether you need an easy-to-write high-level language like
Python or a super fast language like Rust or C that knows how to work with
low-level computer resources in an efficient way.

### Readability

Very often, _readability is the opposite of convenience_. Make wrong choices,
and you'll end up with _obfuscation_.

Also, remember that _simplicity can be deceiving_. Familiar things may seem
simple just because you already know what they mean.

Consider **feature multiplicity** and use it with caution. It feels good and
flexible but can significantly impair code readability.

**Orthogonality** is another important concern and the example we give here
speaks to my heart. In C, the array type is pretty much a syntactic sugar to
pointer type: it cannot be accepted and returned by functions (only the pointer
to array can). This leads to a lot of confusion, especially for those coming
from higher-level languages like Java and Python.

### Abstraction

The overall trend in the programming language development is to strive for
greater abstraction which will allow us to be less imperative without damaging
the efficiency too much. You can see us moving away from ASM and C towards
Haskell, Python, and even Rust (which allows low-level access with a great level
of cost-free abstractions).

The `goto` is almost completely forgotten and replaced by function calls. We let
compilers worry about communicating with the machine. In 1980s, games
programmers understoon _every single feature_ of their target machine to squeeze
as much as possible out of it. This is practically impossible today due to all
these new hardware concepts like pipelining, memory models, multicore
architectures, multi-level cache, etc...

### Efficiency

Oftentimes certain language features are difficult to implement efficiently.
Good compilers will often do a better job of optimising your code than you could
hope to. However, it does not mean that you can give up on writing efficient
algorithms and data-structures alltogether!
