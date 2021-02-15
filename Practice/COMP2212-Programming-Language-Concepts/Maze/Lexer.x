{ 
module Lexer where 
}

%wrapper "posn" 

$digit = 0-9

tokens :-
  $white+           ; 
  "--".*            ; 
  $digit+           { \pos s -> TokenInt     pos (read s) } 
  "if obstacle in"  { \pos s -> TokenIf      pos          }
  then              { \pos s -> TokenThen    pos          }
  else              { \pos s -> TokenElse    pos          }
  fi                { \pos s -> TokenFi      pos          }
  rotate            { \pos s -> TokenRotate  pos          }
  left              { \pos s -> TokenLeft    pos          }
  right             { \pos s -> TokenRight   pos          }
  forward           { \pos s -> TokenForward pos          }

{ 
data Token
  = TokenInt        AlexPosn Int
  | TokenIf         AlexPosn
  | TokenThen       AlexPosn
  | TokenElse       AlexPosn
  | TokenFi         AlexPosn
  | TokenRotate     AlexPosn
  | TokenLeft       AlexPosn
  | TokenRight      AlexPosn
  | TokenForward    AlexPosn
  deriving (Eq,Show) 

tokenize = alexScanTokens

tokenPosn :: Token -> String
tokenPosn (TokenInt     pos _) = showPosn pos
tokenPosn (TokenIf      pos)   = showPosn pos
tokenPosn (TokenThen    pos)   = showPosn pos
tokenPosn (TokenElse    pos)   = showPosn pos
tokenPosn (TokenFi      pos)   = showPosn pos
tokenPosn (TokenRotate  pos)   = showPosn pos
tokenPosn (TokenLeft    pos)   = showPosn pos
tokenPosn (TokenRight   pos)   = showPosn pos
tokenPosn (TokenForward pos)   = showPosn pos

showPosn :: AlexPosn -> String
showPosn (AlexPn _ x y) = "line " ++ show x ++ ", column " ++ show y
}
