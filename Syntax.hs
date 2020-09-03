{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Syntax where

import qualified Parser.AST as Parsed
import Error

import Data.HashMap.Strict as HashMap
import Data.ByteString.Short
import Data.HashSet as HashSet
import Control.Monad.Except

data Var where
  TmVar :: Int -> Var
  TyVar :: Int -> Var
  KiVar :: Int -> Var

type Type = Term
type Name = ShortByteString


data Term where
  TopVar    :: Name -> Term
  LocVar    :: Int -> Term
  Star      :: Term
  Box       :: Term
  Pr1       :: Term -> Term
  Pr2       :: Term -> Term
  Beta      :: Term -> Term -> Term
  IndAbsurd :: Type -> Term -> Term
  Symm      :: Term -> Term
  App       :: Term -> Term -> Term
  AppEr     :: Term -> Term -> Term
  ReWrite   :: Term -> Term  -> Term -> Term
  ForAll    :: Term -> Term -> Term
  Pi        :: Type -> Type -> Term
  Iota      :: Type -> Type -> Type
  Lam       :: Type -> Term -> Term
  LamEr     :: Type -> Term -> Term
  Both      :: Term -> Term -> Type -> Term
  Phi       :: Term -> Term -> Term -> Term
  Let       :: Term -> Type -> Term -> Term
  Eq        :: Term -> Term -> Type
  deriving (Eq, Show)

fromParsedTerm
  :: forall m . MonadError CDLEError m
  => HashSet Name -> (Name, Parsed.Term) -> m Term
fromParsedTerm topDefs (name, term) = go 0 mempty term
  where
    go :: Int -> HashMap Name Int -> Parsed.Term -> m Term
    go depth locNames =
      let
     -- recurse without going under binder.
        go' = go depth locNames
     -- when we go under a binder.
        go'' var t =
          let
            depth'    = depth + 1
            locNames' = HashMap.insert var depth locNames
          in
            go depth' locNames' t
      in \case
      Parsed.Var varStr -> do
        case HashSet.member varStr topDefs of
          True  -> pure $ TopVar varStr
          False -> case HashMap.lookup varStr locNames of
            Just i -> pure $ LocVar i
            Nothing -> throwError $ ScopeError varStr name
      Parsed.Star  -> pure Star
      Parsed.Box   -> pure Box
      Parsed.Pr1 t -> Pr1 <$> go' t
      Parsed.Pr2 t -> Pr2 <$> go' t
      Parsed.Beta eq t -> Beta <$> go' eq <*> go' t
      Parsed.IndAbsurd ty ab -> IndAbsurd <$> go' ty <*> go' ab
      Parsed.Symm eq -> Symm <$> go' eq
      Parsed.App f a -> App <$> go' f <*> go' a
      Parsed.AppEr f a -> AppEr <$> go' f <*> go' a
      Parsed.Phi eq lhs rhs -> Phi <$> go' eq <*> go' lhs <*> go' rhs
      Parsed.Eq lhsP rhsP -> Eq <$> go' lhsP <*> go' rhsP
   -- binding forms
      Parsed.ReWrite eq var mot t ->
        ReWrite <$> go' eq <*> go'' var mot <*> go' t
      Parsed.ForAll var dom dep ->
        ForAll <$> go' dom <*> go'' var dep
      Parsed.Pi var dom dep ->
        Pi <$> go' dom <*> go'' var dep
      Parsed.Iota var dom dep ->
        Iota <$> go' dom <*> go'' var dep
      Parsed.Lam var ty body ->
        Lam <$> go' ty <*> go'' var body
      Parsed.LamEr var ty body ->
        LamEr <$> go' ty <*> go'' var body
      Parsed.Both lhs rhs var rhsTy ->
        Both <$> go' lhs <*> go' rhs <*> go'' var rhsTy
      Parsed.Let var letT letTy body ->
        Let <$> go' letT <*> go' letTy <*> go'' var body 

        
        
      
    

type ErType = ErTerm
data ErTerm where
  ErTopVar :: Name -> ErTerm
  ErLocVar :: Int -> ErTerm
  ErStar   :: ErTerm
  ErBox    :: ErTerm
  ErApp       :: ErTerm -> ErTerm -> ErTerm
  ErForAll    :: ErTerm -> ErTerm -> ErTerm
  ErPi        :: ErType -> ErType -> ErTerm
  ErIota      :: ErType -> ErType -> ErType
  ErLam       :: ErType -> ErTerm -> ErTerm
  ErLet       :: ErTerm -> ErType -> ErTerm -> ErTerm
  ErEq        :: ErTerm -> ErTerm -> ErType
  deriving (Show, Eq)


{-

erase :: Term -> ErTerm
erase = \case
  TopVar s -> ErTopVar s
  Star     -> ErStar
  
  -}
  
{-

class HasLambda t where
  topVar :: String -> t
  locVar :: Var    -> t
  star   :: t
  box    :: t
  app    :: t -> t -> t
  forall :: t -> t -> t
  pi     :: t -> t -> t
  iota   :: t -> t -> t
  lam    :: t -> t -> t
  let_   :: t -> t -> t
  eq     :: t -> t -> t
-}
