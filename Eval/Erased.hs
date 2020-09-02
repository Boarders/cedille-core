{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Eval.Erased where

import Syntax
import Data.HashMap.Strict as HashMap



lookupLocEnv :: Int -> LocEnv -> Maybe Value
lookupLocEnv n env  = go n env
  where
  go :: Int -> LocEnv -> Maybe Value
  go _ [] = Nothing
  go 0 (x : _) = Just x
  go i (_ : xs) = go (i - 1) xs

extendLocEnv :: Value -> LocEnv -> LocEnv
extendLocEnv = (:)

type LocEnv = [Value]
type TopEnv = HashMap Name Value
type Env = (TopEnv, LocEnv)
data Closure = Closure LocEnv ErTerm

data Value where
  VNeutral :: Int -> [Value] -> Value
  VStar    :: Value
  VBox     :: Value
  VForAll  :: Value -> Closure -> Value
  VPi      :: Value -> Closure -> Value
  VIota    :: Value -> Closure -> Value
  VLam     :: Value -> Closure -> Value
  VEq      :: Value -> Value   -> Value


eval :: Env -> ErTerm -> Value
eval (top, loc) = go loc
  where
    go :: LocEnv -> ErTerm -> Value
    go locEnv =
      let
        localEval  = go locEnv
      in \case
      ErTopVar name -> evalTopVar top    name
      ErLocVar var  -> evalLocVar locEnv var
      ErStar        -> VStar
      ErBox         -> VBox
      ErApp f a     -> applyV (localEval f) (localEval a)
      ErForAll dom dep -> VForAll (localEval dom) (Closure locEnv dep)
      ErPi dom dep   -> VPi (localEval dom) (Closure locEnv dep)
      ErIota lhs dep -> VIota (localEval lhs) (Closure locEnv dep)  
      ErLam ty body  -> VLam  (localEval ty)  (Closure locEnv body)
      ErLet def _ body ->
        let
          defV = localEval def
          locEnv' = extendLocEnv defV locEnv
        in
          go locEnv' body
      ErEq lhs rhs -> VEq (localEval lhs) (localEval rhs)


evalTopVar :: TopEnv -> Name -> Value
evalTopVar topEnv name = maybe (evalTopErr "eval" name) id $ HashMap.lookup name topEnv

evalLocVar :: LocEnv -> Int -> Value
evalLocVar locEnv n = maybe (VNeutral n []) id $ lookupLocEnv n locEnv

applyV :: Value -> Value -> Value
applyV = undefined


evalTopErr :: String -> Name -> Value
evalTopErr funName name =
  error $
       funName
    ++ ": Internal Scope Error, toplevel variable"
    ++ show name
    ++ "not found."
          



  
