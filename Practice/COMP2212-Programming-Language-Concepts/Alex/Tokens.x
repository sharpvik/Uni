{ 
module Tokens where 
}

%wrapper "posn" 

$digit = 0-9     
$alpha = [a-zA-Z]    

tokens :-
    $white+         ; 
    "--".*          ; 
    let             { \pos s -> TokenLet pos } 
    in              { \pos s -> TokenIn pos }
    $digit+         { \pos s -> TokenInt pos (read s) } 
    \=              { \pos s -> TokenEq pos }
    \+              { \pos s -> TokenPlus pos }
    \-              { \pos s -> TokenMinus pos }
    \*              { \pos s -> TokenTimes pos }
    \^              { \pos s -> TokenPow pos }
    \/              { \pos s -> TokenDiv pos }
    \(              { \pos s -> TokenLParen pos }
    \)              { \pos s -> TokenRParen pos }
    $alpha [$alpha $digit \_ \’]*   { \pos s -> TokenVar pos s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token
  = TokenLet AlexPosn 
  | TokenIn AlexPosn  
  | TokenInt AlexPosn Int     
  | TokenVar AlexPosn String   
  | TokenEq AlexPosn          
  | TokenPlus AlexPosn        
  | TokenMinus AlexPosn       
  | TokenTimes AlexPosn       
  | TokenPow AlexPosn
  | TokenDiv AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  deriving (Eq,Show) 

tokenPosn (AlexPn _ x y) = show x ++ ":" ++ show y
}

