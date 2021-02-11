{ 
module Tokens where 
}

%wrapper "basic" 

$digit = 0-9     
$alpha = [a-zA-Z]    

tokens :-
    $white+         ; 
    "--".*          ; 
    let             { \s -> TokenLet } 
    in              { \s -> TokenIn }
    $digit+         { \s -> TokenInt (read s) } 
    \=              { \s -> TokenEq }
    \+              { \s -> TokenPlus }
    \-              { \s -> TokenMinus }
    \*              { \s -> TokenTimes }
    \^              { \s -> TokenPow }
    \/              { \s -> TokenDiv }
    \(              { \s -> TokenLParen }
    \)              { \s -> TokenRParen }
    $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token
  = TokenLet          
  | TokenIn           
  | TokenInt Int     
  | TokenVar String   
  | TokenEq          
  | TokenPlus        
  | TokenMinus       
  | TokenTimes       
  | TokenPow
  | TokenDiv         
  | TokenLParen      
  | TokenRParen       
  deriving (Eq,Show) 
}

