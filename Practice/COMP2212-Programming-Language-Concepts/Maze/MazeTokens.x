{ 
module MazeTokens where 
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

tokenPosn (AlexPn _ x y) = show x ++ ":" ++ show y
}
