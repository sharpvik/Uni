{ 
module Parser where 
import Lexer 
}

%name parse 
%tokentype { Token } 
%error { parseError }
%token 
  '->'      { TokenRArr   pos    }
  '<'       { TokenLT     pos    }
  '+'       { TokenPlus   pos    }
  '\\'      { TokenLambda pos    }
  '('       { TokenLBrace pos    }
  ')'       { TokenRBrace pos    }
  ':'       { TokenColon  pos    }
  '='       { TokenAssign pos    }
  let       { TokenLet    pos    }
  in        { TokenIn     pos    }
  if        { TokenIf     pos    }
  then      { TokenThen   pos    }
  else      { TokenElse   pos    }
  type      { TokenType   pos $$ }
  var       { TokenVar    pos $$ }
  bool      { TokenBool   pos $$ }
  int       { TokenInt    pos $$ } 

%left  '<' '+' '\\' 
%right '->' ':' '=' '(' else in

%% 

Exp      : Brack                         { $1 }
         | let TypedVar '=' Exp in Exp   { LetExp $2 $4 $6 }
         | if Exp then Exp else Exp      { IfExp $2 $4 $6 }
         | '\\' TypedVar Exp             { LambdaExp $2 $3 }
         | Exp Brack                     { AppExp $1 $2 }
         | Exp '<' Exp                   { LessThanExp $1 $3 } 
         | Exp '+' Exp                   { PlusExp $1 $3 }
         | var                           { Var $1 }
         | bool                          { Bool $1 }
         | int                           { Int $1 }

Brack    : '(' Exp ')'                   { $2 }

TypedVar : '(' var  ':' Type ')'         { TypedVar $2 $4 }

Type     : type '->' Type                { ($1 : $3) }
         | type                          { [$1] }

{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 

data Exp
  = LessThanExp Exp Exp
  | PlusExp Exp Exp
  | IfExp Exp Exp Exp
  | LambdaExp TypedVar Exp
  | LetExp TypedVar Exp Exp
  | BrackExp Exp
  | AppExp Exp Exp
  | Var String
  | Bool Bool
  | Int Int
  deriving Show 

data TypedVar 
  = TypedVar String Type
  deriving Show

type Type = [String]
}
