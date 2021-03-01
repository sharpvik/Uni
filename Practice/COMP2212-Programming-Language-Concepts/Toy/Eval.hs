module Eval where

import qualified Data.HashMap as HM
-- import qualified Debug.Trace as Trace

import Lexer
import Parser

eval :: String -> Maybe Exp
eval = exec HM.empty . parse . tokenize
-- eval = Just . parse . tokenize

type Env = HM.Map String Exp

exec :: Env -> Exp -> Maybe Exp
exec env (LessThanExp e1 e2) = do
    (Int i1) <- exec env e1
    (Int i2) <- exec env e2
    Just $ Bool $ i1 < i2 
exec env (PlusExp e1 e2) = do
    (Int i1) <- exec env e1
    (Int i2) <- exec env e2
    Just $ Int $ i1 + i2 
exec env (IfExp bg e1 e2) = do
    (Bool b) <- exec env bg
    o1 <- exec env e1
    o2 <- exec env e2
    Just $ if b then o1 else o2
exec _ lambda@(LambdaExp _ _) = Just lambda
exec env (LetExp (TypedVar var _type) from into) = do
    val <- exec env from
    let newEnv = HM.insert var val env
    exec newEnv into
exec env (BrackExp expr) = exec env expr
exec env (AppExp func argExp) = do
    (LambdaExp (TypedVar bind _type) body) <- exec env func
    arg <- exec env argExp
    let newEnv = HM.insert bind arg env
    exec newEnv body
exec env (Var var) = HM.lookup var env
exec _ e@(Bool b) = Just e
exec _ e@(Int i) = Just e
