module Calculus.Language.Syntax (
    Type (..)
  , Term (..)
  , Context
  , mkCtx
  , findCtxBinding
  ) where

{-

| This module defines the syntax (types, terms, context) of the calculus.

-}

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Types.List as L

{- | For convenience. -}
type Name = String

{- | The types allowed in the calculus. -}
data Type =
    ArrowT (S.ArrowType Type)
  | BaseT B.BaseType
  | OptionT (O.OptionType Type)
  | RecordT (R.RecordType Type)
  | ListT (L.ListType Type)
  deriving (Eq, Ord)

instance Show Type where
  show (ArrowT binding) = show binding
  show (BaseT binding) = show binding
  show (OptionT binding) = show binding
  show (RecordT binding) = show binding
  show (ListT binding) = show binding

{- | The terms allowed in the calculus. -}
data Term =
    Var S.VarTerm
  | Abstr (S.AbstrTerm Type Term)
  | Appl (S.ApplTerm Term)
  | Base B.BaseTerm
  | Option (O.OptionTerm Term Type)
  | Record (R.RecordTerm Term Type)
  | List (L.ListTerm Term Term Type)
  deriving (Eq, Ord)

instance Show Term where
  show (Var term) = show term
  show (Base term) = show term
  show (Abstr term) = show term
  show (Appl term) = show term
  show (Option term) = show term
  show (Record term) = show term
  show (List term) = show term

{- | A context is a mapping of names to types. -}
data Context = Context (Map.Map Name Type)

instance Show Context where
  show ctx = 
    case ctx of
      Context bindings ->
        let fmtBinding (name, binding) = name ++ " : " ++ (show binding)
            prettierBindings = map fmtBinding (Map.toList bindings)
        in intercalate ", " prettierBindings

{- | Creates a context from a list of name/type pairs. -}
mkCtx :: [(Name, Type)] -> Context
mkCtx bindings = Context $ Map.fromList bindings

{- | In the context, find the type bound to a name (if any). -}
findCtxBinding :: Context -> Name -> Maybe Type
findCtxBinding ctx name =
  case ctx of
    Context bindings -> 
      Map.lookup name bindings
