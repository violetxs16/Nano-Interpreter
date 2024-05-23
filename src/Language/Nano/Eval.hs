{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude, extendEnv
  , parse, evalOp
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Data.Bifoldable (bifoldl1)
import Foreign (toBool)

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval _ (EInt n) = VInt n
eval env (EVar s) = lookupId s env
eval _ (EBool b) = VBool b
eval env (EBin op v1 v2) = evalOp op (eval env v1) (eval env v2)
eval env (EIf condOrig v1 v2) = if ifHelper (eval env condOrig) then eval env v1 else eval env v2
  where ifHelper :: Value -> Bool
        ifHelper (VBool b) = b
        ifHelper _ = throw (Error ("type error in EIf"))
  --let cond = eval env condOrig in if cond then (eval env v1) else (eval env v2)
--eval _ (EIf _ _ _) = throw (Error ("type error in EIf")) --avoiding catch all case
--eval _ (EBin _ _ _) = throw (Error ("type error")) --avoiding catch all case
eval env (ELet id v1 v2) = 
  let newEnv = extendEnv id x env 
      x = eval newEnv v1 
  in eval newEnv v2 
--eval env (ELet id v1 v2) = eval env' v2
  --where 
    --env' = (extendEnv id (eval env v1) env)
--eval _ (ELet _ _ _) = throw (Error ("type error")) --avoiding catch all case

eval env (ELam id v1) = VClos env id v1

--eval env (EApp v1 v2) = appHelper v1 v2 env (eval env v1)
eval env (EApp funcExpr argExpr) = appHelper (eval env funcExpr)
  where appHelper :: Value -> Value
        appHelper (VClos closureEnv id closureBody ) = eval (extendEnv id (eval env argExpr) closureEnv) closureBody
        appHelper (VPrim f) = f (eval env argExpr)
        appHelper _ = throw (Error ("type error"))
eval _ ENil = VNil




--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus (VInt v1) (VInt v2) = VInt ((+) v1 v2)
evalOp Plus _ _ = throw (Error ("type error in plus operation"))
evalOp Minus (VInt v1) (VInt v2) = VInt ((-) v1 v2)
evalOp Minus _ _ = throw (Error ("type error in minus operation"))
evalOp Mul (VInt v1) (VInt v2) = VInt ((*) v1 v2)
evalOp Mul _ _ = throw (Error ("type error in multiply operation"))
evalOp Eq x y = VBool (x == y)
evalOp Ne x y = VBool (x /= y) 
evalOp Lt (VInt v1) (VInt v2) = VBool (v1 < v2)
evalOp Lt _ _ = throw (Error ("type error in less than"))
evalOp Le (VInt v1) (VInt v2) = VBool (v1 <= v2)
evalOp Le _ _ = throw (Error ("type error in less than or equal to"))
evalOp And (VBool v1) (VBool v2) = VBool (v1 && v2)
evalOp And _ _  = throw (Error ("type error in And "))
evalOp Or (VBool v1) (VBool v2) = VBool (v1 || True)
evalOp Or _ _ = throw (Error ("type error in Or"))
evalOp Cons v1 v2 = VPair v1 v2



--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId id [] = throw (Error ("Unbound variable: " ++ id))
lookupId id ((key, val):xs) = if id == key then val else lookupId id xs


--------------------------------------------------------------------------------
-- | `extendEnv x v env` extends the environment by binding the value `v` to
--   the variable `x`
--------------------------------------------------------------------------------
extendEnv :: Id -> Value -> Env -> Env
--------------------------------------------------------------------------------
extendEnv x v env = (x,v):env
vHead :: Value -> Value
vHead (VPair v1 v2) = v1
vHead _ = throw (Error ("type error"))

vTail :: Value -> Value
vTail (VPair v1 v2) = v2
vTail _ = throw (Error ("type error"))

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
    --head :: (Value -> Value) -> Value -> Value
    ("head", VPrim vHead),
    ("tail", VPrim vTail)
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
