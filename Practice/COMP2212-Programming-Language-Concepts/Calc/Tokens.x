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

tokenPosn :: Token -> String
tokenPosn (TokenLet pos) = showPosn pos
tokenPosn (TokenIn pos) = showPosn pos
tokenPosn (TokenInt pos _) = showPosn pos
tokenPosn (TokenVar pos _) = showPosn pos
tokenPosn (TokenEq pos) = showPosn pos
tokenPosn (TokenPlus pos) = showPosn pos
tokenPosn (TokenMinus pos) = showPosn pos
tokenPosn (TokenTimes pos) = showPosn pos
tokenPosn (TokenPow pos) = showPosn pos
tokenPosn (TokenDiv pos) = showPosn pos
tokenPosn (TokenLParen pos) = showPosn pos
tokenPosn (TokenRParen pos) = showPosn pos

showPosn :: AlexPosn -> String
showPosn (AlexPn _ x y) = "line " ++ show x ++ ", column " ++ show y
}

