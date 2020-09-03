{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
module Parser.AST
  ( Module(..)
  , type Defs
  , Def(..)
  , Term(..)
  )
  where

import Data.ByteString.Short as Short
import Data.String (toString)

type Name = ShortByteString

type Type = Term

data Module =
  Module
  { moduleName :: Name
  , defintions :: Defs
  }
  deriving (Eq, Show)

type Defs = [Def]

data Def = Def
  { defName :: Name
  , defTerm :: Term
  }
  deriving (Eq, Show)

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
  Phi       :: Term -> Term -> Term -> Term
  ForAll    :: Name -> Term -> Term -> Term
  Pi        :: Name -> Type -> Type -> Term
  Iota      :: Name -> Type -> Type -> Type
  Lam       :: Name -> Type -> Term -> Term
  LamEr     :: Name -> Type -> Term -> Term
  Both      :: Term -> Term -> Name -> Type -> Term
  Let       :: Name -> Term -> Type -> Term -> Term
  Eq        :: Term -> Term -> Type
  deriving (Eq, Show)

termToString :: Term -> String
termToString = \case
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
  Phi       :: Term -> Term -> Term -> Term
  ForAll    :: Name -> Term -> Term -> Term
  Pi        :: Name -> Type -> Type -> Term
  Iota      :: Name -> Type -> Type -> Type
  Lam       :: Name -> Type -> Term -> Term
  LamEr     :: Name -> Type -> Term -> Term
  Both      :: Term -> Term -> Name -> Type -> Term
  Let       :: Name -> Term -> Type -> Term -> Term
  Eq        :: Term -> Term -> Type
defToString :: Def -> String
defToString Def{..} =
  fold
    [(show defName) <> " = " termToString]
  
moduleToString :: Module -> String
moduleToString Module{..} =
  fold
    ["module: " <> (show moduleName)]
  where
    defsStr = unlines . fmap defToString
