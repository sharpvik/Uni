# Lexing in Haskell

## The Alex Tool

The Alex tool is a code generation tool that automatically generates lexers in
Haskell. To use it, you must provide an Alex specification (a list of lexemes
and a tokenisation action for each lexeme).

Alex generates Haskell function named `alexScan` that does the job of a scanner
but also identifies tokens and actions to be taken.

Alex is parametrisable in the way it scans and evaluates; in order to customise
it you can provide implementations for the following:

- `type AlexInput`
- `alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)`
- `alexInputPrevChar :: AlexInput -> Char`

And provide an evaluator function `alexScanTokens` that does the evaluation.

## The Basic Wrapper

The following is a simple way of getting a function of type `String -> [String]`
for lexical analysis.

```hs
type AlexInput = (Char, [Byte], String)
--   ^ this type is already defined by Alex
-- prev char
-- rest of the bytes for the current Char
-- rest of the input string

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (c, [], []) = Nothing
alexGetByte (_, [], (c:s)) =
    case utf8Encode c of
        (b:bs) -> Just (b, (c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

alexScanTokens :: String -> [Token]
alexScanTokens str = go ('\n', [], str)
    where go inp@(_, _bs, str) =
        case alexScan inp 0 of
            AlexEOF -> []
            AlexError _ -> error "lexical error"
            AlexSkip inp' len -> go inp'
            AlexToken inp' len act -> act (take len str) : go inp'
```

## Anatomy of an Alex File

By convention, Alex files are saved with the `.x` suffix.

```x
{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z-A-Z]

tokens :-

$white+                                 ;
"--".*                                  ;
let                                     { \s -> Let }
in                                      { \s -> In }
$digit+                                 { \s -> Int (read s) }
[\=\+\-\*\/\)\)]                        { \s -> Sym (head s) }
$alpha [$alpha $digit \_ \']*           { \s -> Var s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token
    = Let
    | In
    | Sym Char
    | Var String
    | Int Int
    deriving (Eq, Show)
}
```

At the very top of the file, in the curly braces, we write some Haskell code
called _pre-amble_. It will be literally copied into the output as is.

Then you specify the `wrapper` you are going to use (defaults to `basic`) with
a _directive_ (command that begins with a `%` sign).

We can then record some common patterns like `$digit` for later use. These
patters are called macros. These are regex patterns, by the way.

Delimiter is `:-` to begin rules. The name `tokens` is irrelevant. After the
delimiter we put the actual interesting stuff in two columns. On the left hand
side we put patterns that we wish to match, and on the right hand side we put
instructions as to how to interpret them (actions for the evaluator).

Following all of these patterns we have the _post-amble_ where we specify the
`Token` type to be used. Just like _pre-amble_, it will be copied into the
output literally.

## Wrappers

There are multiple pre-defined wrappers available:

- `posn` (you're likely to use this one)
- `monad`
- `monadUserState`
- `ByteString`

The `posn` wrapper keeps track of the row and column position to signal it in
case of error.
