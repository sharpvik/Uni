# Extent and Scope

## Extent

The **extent** of a variable (aka lifetime) refers to the periods of execution
time for which it is bound to a particular location storing a meaningful value.
A running program may enter and leave a given extent many times, as in the case
of a clojure.

It is a semantic concept and depends on the _execution model_.

The different kind of variables have different extent. A `static` variable will
stay alive for the whole duration of a program. You may mutate it but it will
stay there.

Stack-dynamic variables have an extent of a particular stack frame or a
procedure call.

Explicit heap-dynamic variables have an extent from explicit allocation to
deallocation (they live between the `malloc` and `free` calls).

Implicit heap-dynamic variables have an extent from implicit allocation to
implicit deallocation (values may persist in memory but addresses are freed).

## Scope

The **scope** is the part of the code in which you can reference the variable.
In other words, it is the part of the code where a variable's name is
meaningful. For example, if we try to compile this Rust program

```rust
fn world(mut s: String) -> String {
    s.push_str("world");
    s
}

fn main() {
    let hello = String::from("hello");
    let message = world(hello);
    println!("{}", hello);
}
```

it fails with compilation error:

```
error[E0382]: borrow of moved value: `hello`
 --> main.rs:9:20
  |
7 |     let hello = String::from("hello");
  |         ----- move occurs because `hello` has type `std::string::String`,
  |               which does not implement the `Copy` trait
8 |     let message = world(hello);
  |                         ----- value moved here
9 |     println!("{}", hello);
  |                    ^^^^^ value borrowed here after move
```

Even though `hello` was declared in the same scope, Rust has some other semantic
rules which prevent us from using it in that scope after we passed it to another
function. See more on borrow checks in Rust if you are interested. Therefore,
in this case, `hello`s scope is limited by the call to `world(hello)`. The
variable `hello` cannot be used after that move and the compiler will not let
you do that.

**Local variables** are declared within a _block_ -- whatever that might be. In
that case, the block is exactly the scope of that variable if no other scoping
rules apply.

**Static variables** exist within the _global scope_ and can be accessed from
anywhere within a file/module/package/program depending on modularity rules of
a language (some static variables may be declared package-private in some
languages).

We refer to **lexical scope** where scope is aligned to staticall determined
areas of source code e.g. a class definition, a code block, or method body.

## Dynamic Scope

In contrast to lexical scope, some languages also support **dynamic scope** for
variables. Dynamic scope is determined at runtime only as it depends on control
flow. Imagine a stack of value bindings for each variable that is updated with
the control stack.

A variable is in a dynamic scope if its name is meaningful within the bindings
of the current call stack.

This is uncommon in modern programming languages as it flies in the face of
referential transparency. However, LISP and Perl seem to not give a damn and use
it anyways.
