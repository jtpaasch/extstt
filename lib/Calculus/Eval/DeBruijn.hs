module Calculus.Eval.DeBruijn (
    indices
  ) where

{-

| This module handles de Bruijn indices.

Variables internally keep an index. However, this index is set
to 0 by default, and is ignored by the syntax and type checker.

If 'Calculus.Language.Config.showDeBruijnIndices' is set to 'False',
then when you print terms, you do not see the de Bruijn index.
If it is set to 'True', then the indices are displayed for each
variable in brackets.

Suppose the display of de Bruijn indices is turned on, and suppose
that 'h_term' is a term in the calculus. Printing it reveals the
default de Bruijn indices for all variables (set to 0):

>>> h_term
λx : Bool.(λy : Bool.(x[0]))

The index on 'x' is obviously incorrect, since 'x' is bound 2 levels
up, by the outermost abstraction.

To update the indices to the correct depths, use the 'indices' function:

>>> indices h_term
λx : Bool.(λy : Bool.(x[2]))

The evaluator uses this function to update all indices before it
performs beta reduction. This way, variables are not captured.
For example, consider the term 'hy_term' (ignoring indices)

>>> hy_term
(λx : Bool.(λy : Bool.(x))) y

If no indices are used, then beta reducing would lead to capture,
for the argument 'y' would be substituted in place of the innermost
'x', and it would then be bound by 'λy':

>>> λy : Bool.(y)

To display the same thing with indices:

>>> λy : Bool.(y[1])

The evaluator updates the indices in a term though, so that 
capture does not happen:

>>> import qualified Calculus.Eval.Evaluator as Eval
>>> Eval.reduce ctx hy_term
Ok. λy : Bool.(y[0])

The innermost 'y' has an index of 0, indicating that it is free,
rather than 1, which would indicate that it was bound 1 level up
by the nearest binder 'λy'.

If displaying de Bruijn indices is turned off, then it is impossible
to see that the inner term is not bound:

>>> Eval.reduce ctx hy_term
Ok. λy : Bool.(y)

But that is just a feature of the display. The evaluator always
uses indices so that capture does not happen.

-}

import qualified Data.Map as Map
import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Types.List as L
import qualified Calculus.Language.Syntax as Syntax

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
    Syntax.List term' ->
      case L.constructorOf term' of
        L.Empty -> term
        L.Cons head tail ->
          let binding = L.typeOfTerm term'
              head' = injectIndices stack head
              tail' = injectIndices stack tail
          in Syntax.List $ L.mkListTerm (L.Cons head' tail') binding

{- | Triggers the injection of de Bruijn indices into a term. -}
indices :: Syntax.Term -> Syntax.Term
indices term = injectIndices emptyStack term
