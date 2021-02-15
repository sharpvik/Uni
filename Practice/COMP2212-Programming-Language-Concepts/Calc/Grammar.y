{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
  let { TokenLet pos } 
  in  { TokenIn pos } 
  int { TokenInt pos $$ } 
  var { TokenVar pos $$ } 
  '=' { TokenEq pos } 
  '+' { TokenPlus pos } 
  '-' { TokenMinus pos } 
  '*' { TokenTimes pos } 
  '^' { TokenPow pos }
  '/' { TokenDiv pos } 
  '(' { TokenLParen pos } 
  ')' { TokenRParen pos } 

%left  '+' '-' NEG
%right '*' '/' '^' in

%% 

Exp : let var '=' Exp in Exp { Let $2 $4 $6 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '^' Exp            { Pow $1 $3 }
    | Exp '/' Exp            { Div $1 $3 } 
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | int                    { Int $1 } 
    | var                    { Var $1 } 
    
{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 

data Exp
  = Let String Exp Exp 
  | Plus Exp Exp 
  | Minus Exp Exp 
  | Times Exp Exp 
  | Pow Exp Exp
  | Div Exp Exp 
  | Negate Exp
  | Int Int 
  | Var String 
  deriving Show 
}
