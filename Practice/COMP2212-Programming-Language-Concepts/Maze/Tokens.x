{ 
module Tokens where 
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

tokenPosn :: Token -> String
tokenPosn (TokenInt     (AlexPn _ x y) _)
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenIf      (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenThen    (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenElse    (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenFi      (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenRotate  (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenLeft    (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenRight   (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
tokenPosn (TokenForward (AlexPn _ x y))
  = "line " ++ show x ++ ", column " ++ show y
}
