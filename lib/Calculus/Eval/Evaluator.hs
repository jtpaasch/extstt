module Calculus.Eval.Evaluator (
    Result (..)
  , reduce
  ) where

{-

| This module provides an evaluator/interepreter for the calculus.

The main function is 'reduce'. Give it a context and a term,
and it will reduce it as far as possible.

Suppose we have a context 'ctx' with some variables in it:

>>> ctx
w : Bool, x : Bool, y : Bool

Suppose 'fy_term' applies a boolean identity function to 'y':

>>> fy_term
(λx : Bool.(x)) y

Reduce it:

>>> reduce ctx fy_term
y

Variable capture is avoided, since under the hood bound variables
are nameless de Brujin indices. See the comments in the header of
'Calculus.Eval.DeBruijn' for more information about this.

-}

import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Types.List as L
import qualified Calculus.Language.Syntax as Syntax
import qualified Calculus.Language.Check as Check
import qualified Calculus.Eval.DeBruijn as DeBruijn

{- | For convenience/clarity. -}
type Depth = S.Index

{- | The results of evaluation. -}
data Result =
    Err Check.Error
  | Ok Syntax.Term

instance Show Result where
  show (Err e) = "Error. " ++ (show e)
  show (Ok term) = "Ok. " ++ (show term)

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
    Syntax.List term' ->
      case L.constructorOf term' of
        L.Empty -> term
        L.Cons head tail ->
          let binding = L.typeOfTerm term'
              head' = subst depth head replacement
              tail' = subst depth tail replacement
          in Syntax.List $ L.mkListTerm (L.Cons head' tail') binding

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
reduce :: Syntax.Context -> Syntax.Term -> Result
reduce ctx term =
  case Check.getType ctx term of
    Check.Err e -> Err e
    Check.Ok _ -> 
      let indexed_term = DeBruijn.indices term
      in Ok $ reduceFixpoint ctx indexed_term
