module Calculus.Eval.Evaluator (
    reduce
  ) where

{-

| This module provides an evaluator/interepreter for the calculus.

The main function is 'reduce'. Give it a context and a term,
and it will reduce it as far as possible.

Note that this evaluator does not type check the term.
It assumes the term is correctly typed, and just reduces it.

Suppose we have a context 'ctx' with some variables in it:

>>> ctx
w : Bool, x : Bool, y : Bool

Suppose 'fy_term' is a boolean identity function:

>>> fy_term
(λx : Bool.(x[0])) y[0]

Reduce it:

>>> reduce ctx fy_term
y[0]

-}

import qualified Data.Map as Map
import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Language.Syntax as Syntax

{- | For convenience/clarity. -}
type Depth = Int

{- | A stack maps indices to variable names. -}
type Stack = [(S.Index, S.Name)]

{- | An empty stack. -}
emptyStack :: Stack
emptyStack = []

{- | Push a name on the front of the stack, bump all other indices. -}
pushOnStack :: S.Name -> Stack -> Stack
pushOnStack name stack =
  let stack' = map (\(k, v) -> (k + 1, v)) stack
  in (1, name):stack'

{- | Find the nearest (least) index for a given name. -}
nearestBinding :: S.Name -> Stack -> Maybe S.Index
nearestBinding name [] = Nothing
nearestBinding name ((k, v):xs) = 
  case v == name of
    True -> Just k
    False -> nearestBinding name xs

{- | Recursively injects de Bruijn indices into a term. -}
injectIndices :: Stack -> Syntax.Term -> Syntax.Term
injectIndices stack term =
  case term of
    Syntax.Var term' ->
      let name = S.nameOfVarTerm term'
      in case nearestBinding name stack of
        Just index -> Syntax.Var $ S.mkVarTermWithIndex name index
        Nothing -> term  
    Syntax.Abstr term' ->
      let name = S.boundNameInAbstr term'
          binding = S.typeOfBoundNameInAbstr term'
          body = S.bodyOfAbstr term'
          stack' = pushOnStack name stack
          body' = injectIndices stack' body
      in Syntax.Abstr $ S.mkAbstrTerm name binding body'
    Syntax.Appl term' ->
      let func = S.funcTermOfAppl term'
          arg = S.argTermOfAppl term'
          func' = injectIndices stack func
          arg' = injectIndices stack arg
      in Syntax.Appl $ S.mkApplTerm func' arg'
    Syntax.Base _ -> term
    Syntax.Option term' ->
      case O.selectionOf term' of
        O.None -> term
        O.Precisely body ->
          let binding = O.typeOfSelection term'
              body' = injectIndices stack body
          in Syntax.Option $ O.mkOptionTerm (O.Precisely body') binding
    Syntax.Record term' ->
      let injectInto field =
            let label = R.labelOfField field
                value = R.valueOfField field
                value' = injectIndices stack value
            in R.mkField value' label
          fields = R.fieldsOfRecordTerm term'
          fields' = map injectInto fields
          recordType = R.typeOfRecordTerm term'
      in Syntax.Record $ R.mkRecordTerm fields' recordType

{- | Triggers the injection of de Bruijn indices into a term. -}
indices :: Syntax.Term -> Syntax.Term
indices term = injectIndices emptyStack term

{- | Substitute a replacement in a given term. -}
subst :: Depth -> Syntax.Term -> Syntax.Term -> Syntax.Term
subst depth term replacement =
  case term of
    Syntax.Var term' ->
      let index = S.indexOfVarTerm term'
      in case index == depth of
        True -> replacement
        False -> term
    Syntax.Abstr term' ->
      let name = S.boundNameInAbstr term'
          binding = S.typeOfBoundNameInAbstr term'
          body = S.bodyOfAbstr term'
          body' = subst (depth + 1) body replacement
      in Syntax.Abstr $ S.mkAbstrTerm name binding body'
    Syntax.Appl term' ->
      let func = S.funcTermOfAppl term'
          arg = S.funcTermOfAppl term'
          func' = subst depth func replacement
          arg' = subst depth arg replacement
      in Syntax.Appl $ S.mkApplTerm func' arg'
    Syntax.Base _ -> term
    Syntax.Option term' ->
      case O.selectionOf term' of
        O.None -> term
        O.Precisely body ->
          let binding = O.typeOfSelection term'
              body' = subst depth body replacement
          in Syntax.Option $ O.mkOptionTerm (O.Precisely body') binding
    Syntax.Record term' ->
      let substInto field =
            let label = R.labelOfField field
                value = R.valueOfField field
                value' = subst depth value replacement
            in R.mkField value' label
          fields = R.fieldsOfRecordTerm term'
          fields' = map substInto fields
          recordType = R.typeOfRecordTerm term'
      in Syntax.Record $ R.mkRecordTerm fields' recordType

{- | Perform one round of beta reduction. -}
reduceOnce :: Syntax.Context -> Syntax.Term -> Maybe Syntax.Term
reduceOnce ctx term =
  case term of
    Syntax.Appl term' ->
      case S.funcTermOfAppl term' of
        Syntax.Abstr term'' -> 
          let body = S.bodyOfAbstr term''
              arg = S.argTermOfAppl term' 
          in Just $ subst 1 body arg
    _ -> Nothing

{- | Repeatedly reduce a term until no more reductions are possible. -}
reduceFixpoint :: Syntax.Context -> Syntax.Term -> Syntax.Term
reduceFixpoint ctx term =
  case reduceOnce ctx term of
    Just term' -> reduceFixpoint ctx term'
    Nothing -> term

{- | Reduce a term as far as possible, given a context. -}
reduce :: Syntax.Context -> Syntax.Term -> Syntax.Term
reduce ctx term =
  let indexed_term = indices term
  in reduceFixpoint ctx indexed_term
