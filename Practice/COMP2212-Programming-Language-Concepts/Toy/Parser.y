{ 
module Parser where 
import qualified Data.HashMap as HM
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
         | '\\' TypedVar Brack           { LambdaExp HM.empty $2 $3 }
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

type Env = HM.Map String Exp

data Exp
  = LessThanExp Exp Exp
  | PlusExp Exp Exp
  | IfExp Exp Exp Exp
  | LambdaExp Env TypedVar Exp
  | LetExp TypedVar Exp Exp
  | BrackExp Exp
  | AppExp Exp Exp
  | Var String
  | Bool Bool
  | Int Int

instance Show Exp where
  show (LessThanExp e1 e2) = show e1 ++ " < " ++ show e2
  show (PlusExp e1 e2) = show e1 ++ " + " ++ show e2
  show (IfExp bg e1 e2) 
    = "if " ++ show bg ++ " then " ++ show e1 ++ " else " ++ show e2
  show (LambdaExp _ tvar exp) = "\\" ++ show tvar ++ " -> " ++ show exp
  show (LetExp tvar e1 e2) 
    = "let " ++ show tvar ++ " = " ++ show e1 ++ " in " ++ show e2
  show (BrackExp exp) = show exp
  show (AppExp func arg) = show func ++ "(" ++ show arg ++ ")"
  show (Var var) = var
  show (Bool b) = show b
  show (Int i) = show i

data TypedVar 
  = TypedVar String Type

instance Show TypedVar where
  show (TypedVar var _) = var

type Type = [String]
}
