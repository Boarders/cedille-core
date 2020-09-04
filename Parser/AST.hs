{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Parser.AST
  ( Module(..)
  , type Defs
  , Def(..)
  , Term(..)
  , moduleToString
  )
  where

import Data.ByteString.Short as Short
import Data.Foldable
import qualified Data.ByteString.Char8 as Char8

type Name = ShortByteString

type Type = Term

data Module =
  Module
  { moduleName :: Name
  , moduleDefs :: Defs
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

isAtomic :: Term -> Bool
isAtomic (Var _) = True
isAtomic Box   = True
isAtomic Star  = True
isAtomic _ = False

bracket :: String -> String
bracket str = "(" <> str <> ")"

-- ()[]{}@,-=:★.12βδςρ∀ΠιλΠ≃
termToString :: Term -> String
termToString = \case
  Var v -> nameToString v
  Star  -> "★"
  Box   -> "☐"
  Pr1 term | isAtomic term -> termToString term <> ".1"
  Pr1 term -> bracket (termToString term) <> ".1"
  Pr2 term | isAtomic term -> bracket (termToString term) <> ".2"  
  Pr2 term -> bracket (termToString term) <> ".2"
  Beta term1 term2 ->
    fold
      [ "β"
      , termToString term1
      , "{"
      , termToString term2
      , "}"
      ]
  IndAbsurd mot absd -> "δ" <> termToString mot <> termToString absd
  Symm eq      -> "ς" <> termToString eq
  App fn a@(App _ _)  -> termToString fn <> bracket (termToString a)
  App fn arg  -> termToString fn <> termToString arg
  AppEr fn arg -> termToString fn <> "-" <> termToString arg
  ReWrite eq name mot term ->
    fold
      [ "ρ"
      , termToString eq
      , "@"
      , nameToString name
      , "."
      , termToString mot
      , "-"
      , termToString term
      ]
  Phi eq lhs rhs     ->
    fold
      [ "φ"
      , termToString eq
      , "-"
      , termToString lhs
      , "{"
      , termToString rhs
      , "}"
      ]
  ForAll name ty term   ->
      fold
      [ "∀"
      , nameToString name
      , ":"
      , termToString ty
      , "."
      , termToString term
      ]
  Pi name ty term ->
    fold
      [ "Π"
      , nameToString name
      , ":"
      , termToString ty
      , "."
      , termToString term
      ]  
  Iota name lhs rhs ->
    fold
      [ "ι"
      , nameToString name
      , ":"
      , termToString lhs
      , "."
      , termToString rhs
      ]
  Lam name ty body       ->
    fold
      [ "λ"
      , nameToString name
      , ":"
      , termToString ty
      , "."
      , termToString body
      ]
  LamEr name ty body     ->
      fold
      [ "Λ"
      , nameToString name
      , ":"
      , termToString ty
      , "."
      , termToString body
      ]
  Both lhs rhs name rhsTy ->
    fold
      [ "["
      , termToString lhs
      , termToString rhs
      , " @ "
      , nameToString name
      , "."
      , termToString rhsTy
      ]
  Let name term ty body    ->
    fold
      [ "["
      , (nameToString name)
      , "="
      , (termToString term)
      , ":"
      , (termToString ty)
      , (termToString body)
      ]
  Eq lhs rhs      -> (termToString lhs) <> "≃" <> (termToString rhs)
defToString :: Def -> String
defToString Def{..} =
  fold
    [(nameToString defName) <> " = " <> termToString defTerm]
  
moduleToString :: Module -> String
moduleToString Module{..} =
  unlines $
    ["module " <> (nameToString moduleName)] <> defsStr
  where
    defsStr = fmap ("\n" <> ) . fmap (<> ";") . fmap defToString $ moduleDefs


nameToString :: Name -> String
nameToString = Char8.unpack . fromShort
  


