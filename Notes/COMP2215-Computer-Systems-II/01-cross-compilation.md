# Cross-Compilation

## Overview

You will most likely be familiar with classical compilation process where you
compile the program on the same machine where you intend to run it.

However, there are different situations where this is not feasible or convenient
to do. For example, you may run a C program on a computer that does not have
memory _at all_. It would be impossible to compile any code on such a machine,
so we have to use another, more "powerful" computer to produce the binary for
the target architecture -- this is called **cross-compilation**.

Another example: what if you used Haskell to write a Haskell compiler for
Windows on a Linux machine for the first time. You won't be able to compile that
compiler on Windows because it requires the Linux-based Haskell compiler. You'll
_have to_ corss-compile.

## Build Process

In the diagram below, the `-->` signifies data being piped through to the next
checkpoint/tool, and `==>` explains what kind of output we receive from a
particular tool.

For example `[Source code *.c] --> Preprocessor ==> [Preprocessed code *.i]`
means that we fed source code file into the Preprocessor and it returned the
preprocessed code file.

```
Build tool (make)
--> Compiler driver (avr-gcc)
    [Source code *.c]
    --> Preprocessor    ==> [Preprocessed code *.i]
    --> Compiler        ==> [Assembly code *.s]
    --> Assembler       ==> [Object file *.o]
    --> Linker          ==> [ELF binary file *.elf]
--> Loader (prepares code for the microcontroller)
    --> avr-objcopy     ==> [Hex-encoded machine code *.hex]
    --> dfu-programmer  --(write)--> microcontroller
```

## Some Info About Files

Object (`*.o`) files are _relocateable_ since they don't have hard-coded
addresses yet, and the label names are still in tact.

Executable objects differ on different OSs: `a.out` on \*nix OSs and `*.exe` on
Windows.

There are also so-called Shared Object Files (`.so`/`.dll`) that can be
dynamically linked to a program at runtime(!) instead of being re-compiled
each time.

## Assembly/ELF Sections

| Section   | Use                       | Location            |
|-----------|---------------------------|---------------------|
| `.text`   | Instructions              | Flash Memory        |
| `.rodata` | Read-only data            | Flash Memory        |
| `.data`   | Initialised global vars   | Flash copied to RAM |
| `.bss`    | Uninitialised global vars | RAM (zeroed)        |

## More On Sections

- **Compiler** assigns program and data to sections taking _scope_ and
  qualifiers like `const` and `static` into account
- **Linker** places the sections to _virtual_ memory
- **Loader** places the sections in _physical_ memory

## Translation Units

C source code ahs three scope types:

1. Program-wide scope
2. File scope (expanded by header `*.h` files)
3. Block scope (restricted to _function scope_ before the `C99` standard)

Programs are organised in files and **can be compiled independently** into
Object Files and **linked later**!

## Use of the ELF Sections

![ELF contents map](media/01-elf-map.png)

Global variables are _zeroed_ because it is cheap to init them to `0000 0000` at
compile time instead of making the programmer do it at runtime explicitly. This
is different for _local_ variables -- C does not initialise local variables for
efficiency reasons. If you don't initialise your local variables in C, they are
assumed to be filled with random bits (based on previous memory location use)
called _garbage_.
