module Calculus.Types.List ( 
    ListType
  , mkListType
  , parameterOfListType
  , Constructor (..)
  , ListTerm
  , mkListTerm
  , constructorOf
  , typeOfTerm
  ) where

{-

| This module provides list types.

Suppose 'bool_type' is a base type:

>>> bool_type
Bool

And suppose 'true' is a term of that type:

>>> true
true

Construct a list of bools:

>>> let boolListT = mkListType bool_type
>>> boollistT
Bool list

Create an empty list of bools:

>>> let emptyBoolList = mkListTerm Empty boolListT
>>> emptyBoolList
[]

Suppose 'emptyList_term' is an empty list term.

>>> emptyList_term
[]

Cons 'true_term' with it, and make a new list:

>>> let cons'd_list = Cons true_term emptList_term
>>> let otherBoolList = mkListTerm cons'd_list boolListT
>>> otherBoolList
true, []

-}

{- | This represents an option type. -}
data ListType a = ListType {
    listTypeParameterizes :: a
  } deriving (Eq, Ord)

instance Show a => Show (ListType a) where
  show binding = (show $ parameterOfListType binding) ++ " list"

{- | Creates a list type. -}
mkListType :: a -> ListType a
mkListType binding = ListType { listTypeParameterizes = binding }

{- | Gets the type the list type parameterizes. -}
parameterOfListType :: ListType a -> a
parameterOfListType = listTypeParameterizes

{- | You can construct an empty list, or cons an element on a list. -}
data Constructor b c =
    Empty
  | Cons b c
  deriving (Eq, Ord)

instance (Show b, Show c) => Show (Constructor b c) where
  show Empty = "[]"
  show (Cons head tail) = (show head) ++ ", " ++ (show tail)

{- | This represents a list term. -}
data ListTerm b c a = ListTerm {
    termConstructor :: Constructor b c
  , termBinding :: ListType a
  } deriving (Eq, Ord)

instance (Show b, Show c, Show a) => Show (ListTerm b c a) where
  show term = show $ constructorOf term

{- | Creates a list term. -}
mkListTerm :: Constructor b c -> ListType a -> ListTerm b c a
mkListTerm term binding =
  ListTerm { termConstructor = term, termBinding = binding }

{- | Gets the constructor of a list term. -}
constructorOf :: ListTerm b c a -> Constructor b c
constructorOf = termConstructor

{- | Gets the type of a list term. -}
typeOfTerm :: ListTerm b c a -> ListType a
typeOfTerm = termBinding
