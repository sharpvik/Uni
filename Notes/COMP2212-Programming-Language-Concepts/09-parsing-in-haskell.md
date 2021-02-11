# Parsing in Haskell

Here, we are going to look at the parser generator called Happy, which is the
Haskell version of `yacc`.

## The Happy Tool

The user provides a Happy specification:

- A list of tokens and grammar rules
- The rules determine valid patterns of tokens along with actions to perform for
  each pattern.

Happy will generate Haskell code with a function name that you specify of type
`[Token] -> T` where `Token` and `T` are both user-definable.

## Anatomy of a Happy File

Similar to Alex files, we have pre-amble and post-amble -- the code enclosed in
curly braces that gets copied literally into the generated `*.hs` file.

The pre-amble is followed by

1. The **name directive** which represents the name of the parse function you
   will receive from the generator
2. The **Token type directive** where you provide tha actual Haskell `Token`
   type (tipically imported from the lexer file)
3. The **error directive** which names the function to call if a parse error is
   encountered in the process. `parseError :: [Token] -> a`

```
%name parseCalc

%tokentype { Token }

%error { parseError }
```

What follows is the **token directive** that gives names to the Token values
seen in the input stream (it's best if these names match those you have used
in Alex but they don't have to).

```
%token
    let     { TokenLet }
    in      { TokenIn }
    int     { TokenInt $$ }
    var     { TokenVar $$ }
    '='     { TokenEq }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }
```

The token directive concludes a list of directives and we put a **mandatory**
delimiter `%%` followed by the grammar rules.

```
%%

Exp     : let var '=' Exp in Exp    { Let $2 $4 $6 }
        | Exp1                      { Exp1 $1 }

Exp1    : Exp1 '+' Term             { Plus $1 $3 }
        | Exp1 '-' Term             { Minus $1 $3 }
        | Term                      { Term $1 }

Term    : Term '+' Factor           { Times $1 $3 }
        | Term '/' Factor           { Div $1 $3 }
        | Factor                    { Factor $1 }

Factor  : int                       { Int $1 }
        | var                       { Var $1 }
        | '(' Exp ')'               { Brack $2 }
```

As you can see, in the grammar rules we've used some predefined token aliases to
ease our workflow.

The grammar rules are followed by some essential post-amble code:

```haskell
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- The following types define the subtypes of the AST.

data Exp
    = Let String Exp Exp
    | Exp1 Exp1
    deriving Show

data Exp1
    = Plus Exp1 Term
    | Minus Exp1 Term
    | Term Term
    deriving Show

data Term
    = Times Term Factor
    | Div Term Factor
    | Factor Factor
    deriving Show

data Factor
    = Int Int
    | Var String
    | Brack Exp
    deriving Show

}
```

## The Grammar (after `%%`)

The grammar section includes a number of **productions** that are rules of how
to rewrite a non-terminal by matching against terminals (patterns of tokens) and
other non-terminals.

Here's the general form of a production:

```
<non-terminal>  : rule1     { action1 }
                | rule2     { action2 }
                ...
                | ruleN     { actionN }
```

where actions are Haskell expressions with occurences of `$n` to represent the
Nth matches value (indexed from 1). Actions specify how to construct an AST node
if a particular rule is met. Of course, you can put more stuff into your
actions, but usually it's just that.

## Entrypoint to the Grammar

You would've noticed in the previous example that there are production rules for
four different non-terminals. So which one of those is the highest-order
non-terminal (entrypoint)?

Well, it's simple -- the first non-terminal is the entrypoint to the grammar.
So be careful -- **order matters**!

## Token Directives

You've seen some weird token directives like this:

```
int     { TokenInt $$ }
var     { TokenVar $$ }
```

The `$$` in this case simply tell Happy that when we match this token, we don't
care about its wrapper type (e.g. `TokenInt`) and we want to work with the
wrapped value instead.

We can also use wildcard `_` to for matching arguments to tokens that you don't
need to further use like this:

```
bool    { TokenBool _ }
```

## Returining Other Datatypes

As we've mentioned previously, actions don't necessarily have to construct an
AST -- you can return _any Haskell value_. You can even evaluate as you parse:

```
Exp     : Exp '+' Exp1      { $1 + $3 }
        | Exp '-' Exp1      { $1 - $3 }
        | Exp1              { $1 }
```

... and so on in this fashion.

> The only restriction on actions is **they all have to return values of the
> same type** (e.g. `ASTNode` or `Int` in the inline evaluation case)!

## Ambiguities and Shift/Reduce Conflict

Suppose we wrote a simple grammar such as this:

```
Exp     : Exp '+' Exp       { $1 + $3 }
        | Exp '-' Exp       { $1 - $3 }
        | Exp '*' Exp       { $1 * $3 }
        | Exp '/' Exp       { $1 / $3 }
        | int               { $1 }
```

This grammar is _extremely_ ambiguous and Happy is going to scream at us. When
matching `1 + 2 + 3`, the parser could either **shift** and read the next `+`
token or **reduce** and say that we've found the `Exp '+' Exp` rule. This is a
shift/reduce conflict where a computer wouldn't really know what to do unless
you somehow communicate what exactly you want.

## Precedence

We can use the `%left` and `%right` directives to specify associativity of
operators.

```
%left '+' '-'
%right '*' '/'

%%

Exp     : Exp '+' Exp       { $1 + $3 }
        | Exp '-' Exp       { $1 - $3 }
        | Exp '*' Exp       { $1 * $3 }
        | Exp '/' Exp       { $1 / $3 }
        | int               { $1 }
```

There is also the `%nonassoc` directive we use to mark non-assiciative operators
like `>` for example.

## Context-Dependent Precedence

Occasionally you can't associate a precedence to a particular token as it may
depend on its use in context. To allow for this, you can create a fake token,
assign a precedence to it and allow a rule to inherit the precedence from that
fake token.

Here's an example:

```
%right in
%left '+' '-'
%left '*' '/'
%left NEG

%%

Exp     : let var '=' Exp in Exp        { Let $2 $4 $6 }
        | Exp '+' Exp                   { Plus $1 $3 }
        ...
        | '-' Exp %prec NEG             { Negate $2 }
        ...
```

We're going to use `NEG` for the unary minus. It's at the very bottom so it
binds more tightly than the normal `'-'` and is left-associate. The `%prec`
directives tells us to inherit the precedence from `NEG` and it only applies to
a particular rule.

Then this is how the string will be parsed:

```
-5 * 3 + 1 --> ((-5) * 3) + 1
```

which is exactly what we want.
