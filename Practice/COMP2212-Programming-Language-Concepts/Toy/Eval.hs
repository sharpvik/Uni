module Eval where

import Data.Maybe
import qualified Data.HashMap as HM

import Lexer
import Parser


evalAndShow :: String -> String
evalAndShow = maybe "evaluation error" show . eval


eval :: String -> Maybe Exp
eval = exec HM.empty . parse . tokenize


exec :: Env -> Exp -> Maybe Exp

exec env (LessThanExp e1 e2) = do
    (Int i1) <- exec env e1
    (Int i2) <- exec env e2
    return $ Bool $ i1 < i2 

exec env (PlusExp e1 e2) = do
    (Int i1) <- exec env e1
    (Int i2) <- exec env e2
    return $ Int $ i1 + i2 

exec env (IfExp bg e1 e2) = do
    (Bool b) <- exec env bg
    o1 <- exec env e1
    o2 <- exec env e2
    return $ if b then o1 else o2

exec env (LetExp (TypedVar var _type) from into) = do
    val <- exec env from
    let newEnv = HM.insert var val env
    exec newEnv into

exec env (AppExp func@(LambdaExp _ _ _) argExp) =
    app env func argExp 
exec env (AppExp expr argExp) = do
    func <- exec env expr
    app env func argExp 

exec _ lambda@(LambdaExp _ _ _) = Just lambda
exec env (BrackExp expr) = exec env expr
exec env (Var var) = HM.lookup var env
exec _ e@(Bool b) = return e
exec _ e@(Int i) = return e


app :: Env -> Exp -> Exp -> Maybe Exp
app env (LambdaExp closure (TypedVar var _type) body) argExp = do
    arg <- exec env argExp
    let newClosure = HM.union (HM.insert var arg closure) env
    case body of
        (LambdaExp _ tvar inner) -> return $ LambdaExp newClosure tvar inner
        other -> exec newClosure other
app _ _ _ = error "application on non-lambda type"
