module Calculus.Language.Check (
    Error (..)
  , Result (..)
  , getType
  ) where

{- 

| This module provides utilities for type checking the calculus.

The main function is 'getType'. If you give it a context and a term,
it will try to derive a type for a term. If it succeeds, it will return
the type it derived. If not, it will return an 'Error'.

Suppose we have some variables in the context 'ctx':

>>> ctx
w : Bool, x : Bool, y : Bool

Suppose we have a boolean identity function:

>>> f_term
λx : Bool.(x)

Check it's type:

>>> getType ctx f_term
Ok. Bool -> Bool

Type checking errors are reported. For example, 
suppose 'z_term' is a variable:

>>> z_term
z

Since 'z' is not declared in the context, it won't type check:

>>> getType ctx z_term
Error. No type specified in context for: z

-}

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Types.List as L
import qualified Calculus.Language.Syntax as Syntax

{- | Types of errors that can occur during type checking. -}
data Error = 
    NoType Syntax.Term
    -- ^For when a variable has no type assigned in the context.
  | NotAbstr Syntax.Term
    -- ^For when an application's left side isn't an abstraction.
  | BadArgType Syntax.Term Syntax.Type Syntax.Type
    -- ^For when an argument to a function doesn't have the right type.
  | BadSelection Syntax.Term Syntax.Type Syntax.Type
    -- ^For when an optional value is the wrong type.
  | WrongFields Syntax.Term Syntax.Type
    -- ^For when a record has the wrong fields.
  | BadField Syntax.Term R.Name Syntax.Term Syntax.Type Syntax.Type
    -- ^For when the value of a field in a record has the wrong type.
  | BadHead Syntax.Term Syntax.Term Syntax.Type Syntax.Type
    -- ^For when the head of a list is the wrong type.

instance Show Error where
  show (NoType term) = 
    "No type specified in context for: " ++ (show term)
  show (NotAbstr term) =
    "Expected abstraction on left in: " ++ (show term)
  show (BadArgType term funcInputBinding argBinding) =
    "Argument has type '" ++ (show argBinding) ++ "' " ++
    "but must have type '" ++ (show funcInputBinding) ++ "' " ++
    "in: " ++ (show term)
  show (BadSelection term selectionType optionType) =
    "The option '" ++ (show term) ++ "' " ++
    "has type '" ++ (show selectionType) ++ "' " ++
    "but it should have type '" ++ (show optionType) ++ "'"
  show (WrongFields term binding) =
    "Wrong fields in: " ++ (show term) ++ " : " ++ (show binding)
  show (BadField record name value valueType labelType) =
    "Field '" ++ name ++ " = " ++ (show value) ++ "' " ++
    "has type '" ++ (show valueType) ++ "' but should " ++
    "have type '" ++ (show labelType) ++ "' in: " ++
    (show record)
  show (BadHead list head headType listTypeParameter) =
    "Head '" ++ (show head) ++ "' has type '" ++ (show headType) ++ "' " ++
    "but should have type '" ++ (show listTypeParameter) ++ "' in: " ++
    (show list)

{- | A result type for the type checker to return. -}
data Result =
    Err Error      -- ^If the type check returns an 'Error'.
  | Ok Syntax.Type -- ^If the type check finds a type.

instance Show Result where
  show (Err err) = "Error. " ++ (show err)
  show (Ok result) = "Ok. " ++ (show result)

{- | Derive a term's type, given a context. -}
getType :: Syntax.Context -> Syntax.Term -> Result 
getType ctx term = 
  case term of
    Syntax.Var term' ->
      let name = S.nameOfVarTerm term'
      in case Syntax.findCtxBinding ctx name of
        Just binding -> Ok binding
        Nothing -> Err $ NoType term
    Syntax.Abstr term' ->
      let abstrBinding = S.typeOfBoundNameInAbstr term'
          body = S.bodyOfAbstr term'
      in case getType ctx body of
        Err e -> Err e
        Ok bodyBinding ->
          Ok $ Syntax.ArrowT (S.mkArrowType abstrBinding bodyBinding)
    Syntax.Appl term' ->
      case S.funcTermOfAppl term' of
        Syntax.Abstr func ->
          let abstrInputBinding = S.typeOfBoundNameInAbstr func
              arg = S.argTermOfAppl term'
          in case getType ctx arg of
            Err e -> Err e
            Ok argBinding ->
              case abstrInputBinding == argBinding of
                True -> getType ctx (S.bodyOfAbstr func)
                False -> 
                  Err $ BadArgType term abstrInputBinding argBinding 
        _ -> Err $ NotAbstr term 
    Syntax.Base term' -> 
      let binding = B.typeOfTerm term'
      in Ok $ Syntax.BaseT binding 
    Syntax.Option term' ->
      let binding = O.typeOfSelection term'
      in case O.selectionOf term' of
        O.None -> Ok $ Syntax.OptionT binding
        O.Precisely body -> 
          case getType ctx body of
            Err e -> Err e
            Ok binding' ->
              let typeParameter = O.parameterOfOptionType binding
              in case binding' == typeParameter of
                True -> Ok $ Syntax.OptionT binding
                False -> Err $ BadSelection term binding' typeParameter
    Syntax.Record term' ->
      let binding = R.typeOfRecordTerm term'
          labels = R.labelsOfRecordType binding
          fields = R.fieldsOfRecordTerm term'
          fieldLabels = map (\field -> R.labelOfField field) fields          
      in case (Set.fromList fieldLabels) == (Set.fromList labels) of
        False -> Err $ WrongFields term (Syntax.RecordT binding)
        True ->
          let checkValues [] = Right ()
              checkValues (field:fields) =
                let value = R.valueOfField field
                in case getType ctx value of
                  Err e -> Left e
                  Ok valueType ->
                    let label = R.labelOfField field
                        labelType = R.typeOfLabel label
                    in case valueType == labelType of
                      True -> checkValues fields
                      False -> 
                        let name = R.nameOfLabel label
                            e = BadField term name value valueType labelType
                        in Left e 
          in case checkValues fields of
            Left e -> Err e
            Right _ -> Ok $ Syntax.RecordT binding
    Syntax.List term' ->
      let binding = L.typeOfTerm term'
      in case L.constructorOf term' of
        L.Empty -> Ok $ Syntax.ListT binding
        L.Cons head tail ->
          case getType ctx head of
            Err e -> Err e
            Ok binding' ->
              let typeParameter = L.parameterOfListType binding
              in case binding' == typeParameter of
                False -> Err $ BadHead term head binding' typeParameter
                True -> getType ctx tail
