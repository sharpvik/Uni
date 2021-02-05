# Variable Names and Binding

## Names

Names are essential in programming languages because we manipulate _abstract
entities_ and so we need to _identify_ them.

The identifier rules are really _ad hoc_ and differ in

- case sensitivity
- restricted/fixed length
- enforced lexical rules (must begin with a letter / cannot contain punctuation)

In some languages, your names can overwrite reserved keywords. In others, there
are ways around this (like if you want to call your function `bool` in Rust, you
can use `r#` in front of it to turn it into a _raw identifier_).

We don't know yet whether there is a superior naming convention so we are stuck
(maybe forever) with these different sets of _ad hoc_ rules.

## Variables

The _original_ meaning of variable is that of a memory location whose contents
may change. So, variable `x` is just a _reference_ to some memory location.

Now, with all the developments of high-level programming languages, variables
came to mean a "placeholder for a value of some (possibly complex) type".

Sometimes, variables are not actually _variable_... Coming back to rust where

```rust
let magic = vec![42, 42, 42];
```

is an _immutable_ vector of `i32`s. To allow mutability, you have to declare it
with `let mut`.

In functional languages, variables can store functions and higher-order closures
like `(Int -> Int) -> Int -> Int`.

Some languages still allow you to access the memory location of a variable using
tools like the _reference operator_ (e.g. `&x` in C) or the `Unsafe` class in
Java.

Since variables are just references to memory, there still exists the concept of
**aliasing** where two different variables point to the same location in memory.
This can be a great good and a great evil. Let's examine some C code.

```c
int i = 42;
int* a = &i;
int* b = a;

int magic     (int* i) { return *i; }
int not_found (int* i) { *i = 404;  }
```

What happens if we do this..?

```c
int main(void) {
    not_found(b);
    printf("%d is 42?", magic(a));
}
```

## Variable Attributes

- Name
- Memory address (L-Value; stands for left hand side of variable assignment)
- Value (R-Value)
- Type (type of the stored value)
- Extent (temporal lifespan of a variable; very often related to scope)
- Scope (set of code locations from which a variabe can be accessed)

## Binding

A **binding** is an association between an entity and some attribute (e.g.
between a _variable_ and its _type_ or _scope_).

The bindings are important at compile and runtime, although the importance
differs in different languages. Some languages are so type-agnostic that they
simply _interpret_ values as one type or another as convenient for the operation
at hand (I'm looking at you, JavaScript).

### Static vs Dynamic Binding

**Static binding** occurs before execution (at compile time) and remains
unchanged throughout execution. This approach is commonly taken by languages
that are intended for large-scale software systems where the importance of
compile-time checks is high due to high costs of failure.

**Dynamic binding** occurs at runtime and can therefore change. Many scripting
languages rely on dynamic binding to provide a simple and flexible environment
of execution.

## Allocation and Deallocation

**Allocation** is a process of binding variable to its memory address; it can be
both _static_ and _dynamic_.
**Deallocation** is, therefore, the opposite process where we free the memory
that was previously taken up by the variable. Deallocation is an inherently
dynamic process, however quite often we know a lot about deallocation points
at compile time due to scopeing rules.

These can two processes can be handled by the programmer through _manual memory
management_ like in C/C++ or by the Garbage Collector (GC) like in Java, Python,
Go, Haskell and many others. The popularity of garbage collection is connected
to humans' inherent inability to navigate large state spaces which, in case of
manual memory management leads to _memory leaks_ when you forget to call `free`.

Now that the Moore's Law seems to come to an end -- we are entering a plateau in
terms of the number of transistors in a regular processor chip -- there seems to
be an increase of interest in programming languages that disallow manual memory
management and don't have a GC either. Languages like Rust turn programmer into
a garbage collector by making them provide additional information to the
compiler -- within the source code -- about the _lifetime_ of variables and
`struct` fields.

## The Four Kinds of Variables

1. **Static Variables** (aka _global_ variables) that are bound to a memory
   location at initialisation time
2. **Stack-Dynamic Variables** (aka _local_ variables) that exist on a runtime
   stack; these are usually created during a function call and destroyed as soon
   as that function `return`s
3. **Explicit Heap-Dynamic Variables** are variables that are created and
   destroyed at _runtime_ via explicit commands by the programmer (think `new`
   and `delete` in C++ or `malloc` and `free` in C)
4. **Implicit Heap-Dynamic Variables** where heap memory is allocated at
   variable assignment and re-assignment. This is higly error-prone and
   inefficient, so you don't really see that too often, although it is used in
   LISP, C, and JavaScript (for `array`s)

## Static Type Binding

**Static Type Binding** is usually done through explicit _type declaration_ in
the source code of a program or via _type inference_. Of course, this is just an
illusion, the processor only cares about bites -- not `int`s or `string`s --
however, this allows for the great framework of reasoning about our programs.

**Type Declaration** is used in Ada, ALGOL, Pascal, Cobol, C/C++, Java, etc.

**Type Inference** is used in ML, Haskell, Elm, Rust, etc. Compiler normally
infers the most _generic_ function type using generic types or some other
concept.

## Dynamic Type Binding

**Dynamic Type Binding** typically occurs as a variable is assigned to a value
at runtime. Most scripting languages toy around with it (JavaScript, PHP,
Python, Ruby, etc.).

Dynamic typing has efficiency implications at runtime since variable types have
to be checked for each operation. If you ever wondered why the hell Python is
60 to 200 times slower than C -- that's why. On the other hand, there are
usually readability and coding conveniences linked to it.
