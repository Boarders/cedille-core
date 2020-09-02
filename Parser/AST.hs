{-# LANGUAGE GADTs #-}
module Parser.AST
  ( Term(..)
  )
  where

import Data.ByteString.Short as Short

type Name = ShortByteString

type Type = Term
data Term where
  Var       :: Name -> Term
  Star      :: Term
  Box       :: Term
  Pr1       :: Term -> Term
  Pr2       :: Term -> Term
  Beta      :: Term -> Term -> Term
  IndAbsurd :: Type -> Term -> Term
  Symm      :: Term -> Term
  App       :: Term -> Term -> Term
  AppEr     :: Term -> Term -> Term
  ReWrite   :: Term -> Name -> Type -> Term -> Term
  ForAll    :: Name -> Term -> Term -> Term
  Pi        :: Name -> Type -> Type -> Term
  Iota      :: Name -> Type -> Type -> Type
  Lam       :: Name -> Type -> Term -> Term
  LamEr     :: Name -> Type -> Term -> Term
  Both      :: Term -> Term -> Name -> Type -> Term
  Phi       :: Term -> Term -> Term -> Term
  Let       :: Name -> Term -> Type -> Term -> Term
  Eq        :: Term -> Term -> Type
  
