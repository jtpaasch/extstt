module Calculus.Types.Simple (
    Name
  , Index
  , ArrowType
  , mkArrowType
  , domainTypeOfArrowType
  , rangeTypeOfArrowType
  , VarTerm
  , mkVarTerm
  , mkVarTermWithIndex
  , nameOfVarTerm
  , indexOfVarTerm
  , AbstrTerm
  , mkAbstrTerm
  , boundNameInAbstr
  , typeOfBoundNameInAbstr
  , bodyOfAbstr
  , ApplTerm
  , mkApplTerm
  , funcTermOfAppl
  , argTermOfAppl
  ) where

{-

| This module provides simple types/terms for the calculus.

The simple types/terms in question are those of the
simply typed lambda calculus. 

There is just one type: the arrow (i.e., function) type.
For the terms, there are variables, abstractions, and applications.

Suppose 'bool_type' is a type of the calculus:

>>> bool_type
Bool

Create some variables:

>>> let x = mkVarTerm "x"
>>> let y = mkVarTerm "y"

Create an abstraction (e.g., the identity function):

>>> let f = mkAbstrTerm "x" bool_type x

Create an application:

>>> let fx = mkAppl f x

-}

{- | For convenience. -}
type Name = String
type Index = Int

{- | This represents an arrow/function type. -}
data ArrowType a = ArrowType {
    domainType :: a
  , rangeType :: a
  } deriving (Eq, Ord)

instance Show a => Show (ArrowType a) where
  show binding =
    (show $ domainTypeOfArrowType binding) ++ " -> " ++
    (show $ rangeTypeOfArrowType binding)

{- | Creates an arrow type. -}
mkArrowType :: a -> a -> ArrowType a
mkArrowType domain range = 
  ArrowType {domainType = domain, rangeType = range }

{- | Gets the type of an arrow's domain. -}
domainTypeOfArrowType :: ArrowType a -> a
domainTypeOfArrowType = domainType

{- | Gets the type of an arrow's range. -}
rangeTypeOfArrowType :: ArrowType a -> a
rangeTypeOfArrowType = rangeType

{- | This represents a variable. -}
data VarTerm = VarTerm {
    varTermName :: Name
  , varTermIndex :: Index
  } deriving (Eq, Ord)

instance Show VarTerm where
  show term =
    (nameOfVarTerm term) ++ "[" ++ (show $ indexOfVarTerm term) ++ "]"

{- | Gets the name of a variable. -}
nameOfVarTerm :: VarTerm -> Name
nameOfVarTerm = varTermName

{- | Gets the index of a variable. -}
indexOfVarTerm :: VarTerm -> Index
indexOfVarTerm = varTermIndex

{- | Creates a variable term. -}
mkVarTerm :: Name -> VarTerm
mkVarTerm name = VarTerm { varTermName = name, varTermIndex = 0 }

{- | Creates a variable term with a specified index. -}
mkVarTermWithIndex :: Name -> Index -> VarTerm
mkVarTermWithIndex name idx = 
  VarTerm { varTermName = name, varTermIndex = idx }

{- | This represents an abstraction. -}
data AbstrTerm a b = AbstrTerm {
    boundName :: Name
  , typeOfBoundName :: a
  , abstrBody :: b
  } deriving (Eq, Ord)

instance (Show a, Show b) => Show (AbstrTerm a b) where
  show term =
    "λ" ++ (boundNameInAbstr term) ++
    " : " ++ (show $ typeOfBoundNameInAbstr term) ++ 
    ".(" ++ (show $ bodyOfAbstr term) ++ ")"

{- | Creates an abstraction term. -}
mkAbstrTerm :: Name -> a -> b -> AbstrTerm a b
mkAbstrTerm name binding body = 
  AbstrTerm { 
    boundName = name, 
    typeOfBoundName = binding, 
    abstrBody = body }

{- | Gets the name bound in an abstraction. -}
boundNameInAbstr :: AbstrTerm a b -> Name
boundNameInAbstr = boundName

{- | Gets the binding of an abstraction. -}
typeOfBoundNameInAbstr :: AbstrTerm a b -> a
typeOfBoundNameInAbstr = typeOfBoundName

{- | Gets the body of an abstraction. -}
bodyOfAbstr :: AbstrTerm a b -> b
bodyOfAbstr = abstrBody

{- | This represents an application. -}
data ApplTerm a = ApplTerm {
    funcTerm :: a
  , argTerm :: a
  } deriving (Eq, Ord)

instance Show a => Show (ApplTerm a) where
  show term =
    "(" ++ (show $ funcTermOfAppl term) ++ ") " ++ 
    (show $ argTermOfAppl term)

{- | Creates an application term. -}
mkApplTerm :: a -> a -> ApplTerm a
mkApplTerm func arg = ApplTerm { funcTerm = func, argTerm = arg }

{- | Gets the function term (the left term) of an application. -}
funcTermOfAppl :: ApplTerm a -> a
funcTermOfAppl = funcTerm

{- | Gets the argument term (the right term) of an application. -}
argTermOfAppl :: ApplTerm a -> a
argTermOfAppl = argTerm
