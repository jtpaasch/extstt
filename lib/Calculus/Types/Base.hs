module Calculus.Types.Base (
    Name
  , Value
  , BaseType
  , BaseTerm
  , nameOfType
  , valueOfTerm
  , typeOfTerm
  , mkBaseType
  , mkBaseTerm
  ) where

{-

| This module provides custom base types.

For example, create a custom "Bool" type:

>>> let boolT = mkBaseType "Bool"
>>> boolT
Bool

Then create some inhabitants, say "true" and "false":

>>> let true = mkBaseTerm "true" boolT
>>> true
true

>>> let false = mkBaseTerm "false" boolT
>>> false
false

-}

{- | For convenience. -}
type Name = String
type Value = String

{- | This represents a custom base type. -}
data BaseType = BaseType {
    baseTypeName :: Name -- ^The name of the type.
  } deriving (Eq, Ord)

instance Show BaseType where
  show binding = baseTypeName binding

{- | This represents a term of a base type. -}
data BaseTerm = BaseTerm {
    baseTermValue :: Value      -- ^The value of the term.
  , baseTermBinding :: BaseType -- ^The type its bound to.
  } deriving (Eq, Ord)

instance Show BaseTerm where
  show term = baseTermValue term

{- | Gets the name of a base type. -}
nameOfType :: BaseType -> Name
nameOfType = baseTypeName

{- | Gets the value of a base term. -}
valueOfTerm :: BaseTerm -> Value
valueOfTerm = baseTermValue

{- | Gets the type of a base term. -}
typeOfTerm :: BaseTerm -> BaseType
typeOfTerm = baseTermBinding

{- | Creates a base type with a given name. -}
mkBaseType :: Name -> BaseType
mkBaseType name = BaseType { baseTypeName = name }

{- | Creates a term with the given value, bound to the given base type. -}
mkBaseTerm :: Value -> BaseType -> BaseTerm
mkBaseTerm value binding =
  BaseTerm { baseTermValue = value, baseTermBinding = binding }
