module Calculus.Types.Option (
    OptionType
  , mkOptionType
  , parameterOfOptionType
  , Selection (..)
  , OptionTerm
  , mkOptionTerm
  , selectionOf
  , typeOfSelection
  ) where

{-

| This module provides option types.

Suppose 'bool_type' is a base type. 

>>> bool_type
Bool

Create an optional version of it:

>>> let optBoolT = mkOptionType bool_type
>>> optBoolT
Optional Bool

To construct a term that inhabits this optional bool time,
select a bool from the bool type.

One option is to select no bool at all. To create such an inhabitant:

>>> let no_optBool = mkOptionTerm None optBoolT
>>> no_optBool
Nothing

Another option is to select a particular inhabitant of the bool type.
Suppose 'true_term' is a term that inhabits the 'bool_type'.

>>> true_term
true

To select that particular term for the option:

>>> let true_optBool = mkOptionTerm (Precisely true_term) optBoolT
>>> true_optBool
Precisely true

-}

{- | This represents an option type. -}
data OptionType a = OptionType {
    optionTypeParameterizes :: a
  } deriving (Eq, Ord)

instance Show a => Show (OptionType a) where
  show binding = "Optional " ++ (show $ parameterOfOptionType binding)

{- | Creates an option type. -}
mkOptionType :: a -> OptionType a
mkOptionType binding = OptionType { optionTypeParameterizes = binding }

{- | Gets the type the option type parameterizes. -}
parameterOfOptionType :: OptionType a -> a
parameterOfOptionType = optionTypeParameterizes

{- | You can either select nothing, or some particular term 'b'. -}
data Selection b =
    None
  | Precisely b
  deriving (Eq, Ord)

instance Show b => Show (Selection b) where
  show None = "Nothing"
  show (Precisely term) = "Precisely " ++ (show term)

{- | This represents a term of an option type. -}
data OptionTerm b a = OptionTerm {
    selection :: Selection b
  , selectionBinding :: OptionType a
  } deriving (Eq, Ord)

instance (Show b, Show a) => Show (OptionTerm b a) where
  show term = show $ selectionOf term

{- | Creates an option term. -}
mkOptionTerm :: Selection b -> OptionType a -> OptionTerm b a
mkOptionTerm option binding = 
  OptionTerm { selection = option, selectionBinding = binding }

{- | Gets the selection of an option term. -}
selectionOf :: OptionTerm b a -> Selection b
selectionOf = selection

{- | Gets the option type an option term is bound to. -}
typeOfSelection :: OptionTerm b a -> OptionType a
typeOfSelection = selectionBinding
