{ 
module Grammar where 
import Tokens 
}

%name parseMaze 
%tokentype { Token } 
%error { parseError }
%token 
  int       { TokenInt     pos $$ } 
  if        { TokenIf      pos    }
  then      { TokenThen    pos    }
  else      { TokenElse    pos    }
  fi        { TokenFi      pos    }
  rotate    { TokenRotate  pos    }
  left      { TokenLeft    pos    }
  right     { TokenRight   pos    }
  forward   { TokenForward pos    }

%% 

Prog    : Stmt                          { One $1 } 
        | Stmt Prog                     { Many $1 $2 }

Stmt    : forward int                   { Fwd $2 }
        | rotate Dir                    { Rot $2 }
        | if int then Prog else Prog fi { If $2 $4 $6 }

Dir     : left                          { DLeft }
        | right                         { DRight }

{ 
parseError :: [Token] -> a
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok
parseError _ = error "Parse error" 

data Prog = One Stmt | Many Stmt Prog deriving Show

data Stmt
  = Fwd Int
  | Rot Dir
  | If Int Prog Prog 
  deriving Show 

data Dir = DLeft | DRight deriving (Show, Eq)
}
