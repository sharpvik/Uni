# Toy Language

## Generate Lexer and Parser

```bash
./generate.sh
```

## Build It

```bash
cabal build
```

## Run It

```bash
cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.3/Toy-0.1.0.0/x/Toy/build/Toy/Toy ./

./Toy <filename>
# or
./Toy repl
```
