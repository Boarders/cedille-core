{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}


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
  deriving (Show, Eq)

data Value where
  VNeutral :: Int -> [Value] -> Value
  VStar    :: Value
  VBox     :: Value
  VForAll  :: Value -> Closure -> Value
  VPi      :: Value -> Closure -> Value
  VIota    :: Value -> Closure -> Value
  VLam     :: Value -> Closure -> Value
  VEq      :: Value -> Value   -> Value
  deriving (Show, Eq)


pattern VVar :: Int -> Value
pattern VVar n = VNeutral n []

-- to do:
-- add a threshold for how many times to apply localEval and if threshold is reached
-- then throw a timeout error
eval ::  Env -> ErTerm -> Value
eval (top, loc) = go loc
  where
    go :: LocEnv -> ErTerm -> Value
    go locEnv =
      let
        localEval  = go locEnv
      in \case
      ErTopVar name    -> evalTopVar top    name
      ErLocVar var     -> evalLocVar locEnv var
      ErStar           -> VStar
      ErBox            -> VBox
      ErApp f a        -> applyV top (localEval f) (localEval a)
      ErForAll dom dep -> VForAll (localEval dom) (Closure locEnv dep)
      ErPi dom dep     -> VPi (localEval dom) (Closure locEnv dep)
      ErIota lhs dep   -> VIota (localEval lhs) (Closure locEnv dep)  
      ErLam ty body    -> VLam  (localEval ty)  (Closure locEnv body)
      ErLet def _ body ->
        let
          defV    = localEval def
          locEnv' = extendLocEnv defV locEnv
        in
          go locEnv' body
      ErEq lhs rhs -> VEq (localEval lhs) (localEval rhs)


evalTopVar :: TopEnv -> Name -> Value
evalTopVar topEnv name = maybe (evalTopErr "eval" name) id $ HashMap.lookup name topEnv

evalLocVar :: LocEnv -> Int -> Value
evalLocVar locEnv n = maybe (VNeutral n []) id $ lookupLocEnv n locEnv

evalClosure :: TopEnv -> Closure -> Value -> Value
evalClosure topEnv (Closure env body) argV = eval (topEnv, (extendLocEnv argV env)) body

applyV :: TopEnv -> Value -> Value -> Value
applyV topEnv (VLam _ clos) ~arg       = evalClosure topEnv clos arg
applyV _      (VNeutral stuck sp) ~arg = VNeutral stuck (arg : sp)
applyV _ f _ = error
  $ "Internal error [Eval.Erase.applyV]  " <>
    "cannot apply" <> (show f)

evalTopErr :: String -> Name -> Value
evalTopErr funName name =
  error $
       funName
    ++ ": Internal Scope Error, toplevel variable"
    ++ show name
    ++ "not found."


conv :: TopEnv ->  Value -> Value -> Bool
conv topEnv lhs rhs = go 0 lhs rhs
  where
    go :: Int -> Value -> Value -> Bool
    go fresh v1 v2 = case (v1, v2) of
      (VNeutral h1 sp1, VNeutral h2 sp2) ->
        h1 == h2 &&
        length sp1 == length sp2 &&
        (and $ zipWith (go fresh) sp1 sp2)
      (VStar    , VStar   ) -> True
      (VBox     , VBox    ) -> True 
      (VLam ty1V body1V, VLam ty2V body2V) ->
        let
          openBody1V = evalClosure topEnv body1V (VVar fresh)
          openBody2V = evalClosure topEnv body2V (VVar fresh)
          fresh'     = fresh + 1
        in
          go fresh ty1V ty2V &&
          go fresh' openBody1V openBody2V
      (VLam ty1V body1V, VLam ty2V body2V) ->
        let
          openBody1V = evalClosure topEnv body1V (VVar fresh)
          openBody2V = evalClosure topEnv body2V (VVar fresh)
          fresh'     = fresh + 1
        in
          go fresh ty1V ty2V &&
          go fresh' openBody1V openBody2V          
      (VForAll dom1V dep1V, VForAll dom2V dep2V) -> undefined

      (VPi dom1V dep1V, VPi dom2V dep2V) -> undefined
      (VIota lhs1V rhs1V, VIota lhs2V rhs2V) -> undefined
      (VLam ty1V body1V, VLam ty2V body2V) -> undefined
      (VEq lhs1V rhs1V , VEq lhs2V rhs2V) -> undefined
