{ 
module Lexer where 
}

%wrapper "posn" 

$digit = 0-9
$upper = A-Z
$lower = a-z

tokens :-
  $white+           ; 
  "--".*            ; 
  "->"              { \pos _ -> TokenRArr   pos          }
  "<"               { \pos _ -> TokenLT     pos          }
  "+"               { \pos _ -> TokenPlus   pos          }
  "\"               { \pos _ -> TokenLambda pos          }
  "("               { \pos _ -> TokenLBrace pos          }
  ")"               { \pos _ -> TokenRBrace pos          }
  ":"               { \pos _ -> TokenColon  pos          }
  "="               { \pos _ -> TokenAssign pos          }
  let               { \pos _ -> TokenLet    pos          }
  in                { \pos _ -> TokenIn     pos          }
  if                { \pos _ -> TokenIf     pos          }
  then              { \pos _ -> TokenThen   pos          }
  else              { \pos _ -> TokenElse   pos          }
  true              { \pos s -> TokenBool   pos True     }
  false             { \pos s -> TokenBool   pos False    }
  $upper$lower*     { \pos s -> TokenType   pos s        }
  $lower+           { \pos s -> TokenVar    pos s        }
  $digit+           { \pos s -> TokenInt    pos (read s) } 

{ 
data Token
  = TokenRArr   AlexPosn      
  | TokenLT     AlexPosn      
  | TokenPlus   AlexPosn      
  | TokenLambda AlexPosn      
  | TokenLBrace AlexPosn      
  | TokenRBrace AlexPosn      
  | TokenColon  AlexPosn      
  | TokenAssign AlexPosn      
  | TokenLet    AlexPosn      
  | TokenIn     AlexPosn      
  | TokenIf     AlexPosn      
  | TokenThen   AlexPosn      
  | TokenElse   AlexPosn      
  | TokenBool   AlexPosn Bool 
  | TokenType   AlexPosn String    
  | TokenVar    AlexPosn String    
  | TokenInt    AlexPosn Int
  deriving (Eq,Show) 

tokenize = alexScanTokens

tokenPosn :: Token -> String
tokenPosn (TokenRArr   pos) = showPosn pos     
tokenPosn (TokenLT     pos) = showPosn pos     
tokenPosn (TokenPlus   pos) = showPosn pos     
tokenPosn (TokenLambda pos) = showPosn pos     
tokenPosn (TokenLBrace pos) = showPosn pos     
tokenPosn (TokenRBrace pos) = showPosn pos     
tokenPosn (TokenColon  pos) = showPosn pos     
tokenPosn (TokenAssign pos) = showPosn pos     
tokenPosn (TokenLet    pos) = showPosn pos     
tokenPosn (TokenIn     pos) = showPosn pos     
tokenPosn (TokenIf     pos) = showPosn pos     
tokenPosn (TokenThen   pos) = showPosn pos     
tokenPosn (TokenElse   pos) = showPosn pos     
tokenPosn (TokenBool   pos _) = showPosn pos
tokenPosn (TokenType   pos _) = showPosn pos
tokenPosn (TokenVar    pos _) = showPosn pos
tokenPosn (TokenInt    pos _) = showPosn pos

showPosn :: AlexPosn -> String
showPosn (AlexPn _ x y) = "line " ++ show x ++ ", column " ++ show y
}
