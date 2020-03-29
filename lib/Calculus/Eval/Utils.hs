module Calculus.Eval.Utils (
    freeVars
  , Error (..)
  , Result (..)
  , close
  ) where

{-

| This module provides some extra evaluation utilities.

Suppose we have a context:

>>> ctx
w : Bool, x : Bool, y : Bool

And suppose 'g_term' is a term in the calculus:

>>> g_term
λy : Bool.(x)

Get all free variables in the term:

>>> freeVars g_term
[x]

Close the term (which abstracts all of its free variables):

>>> close ctx g_term
Ok. λx : Bool.(λy : Bool.(x))

-}

import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Language.Syntax as Syntax
import qualified Calculus.Eval.DeBruijn as DeBruijn

{- | Recursively find all free variables in a term. -}
findFreeVars :: [Syntax.Term] -> Syntax.Term -> [Syntax.Term]
findFreeVars acc term =
  case term of
    Syntax.Var term' -> 
      let index = S.indexOfVarTerm term'
      in case index == 0 of
        True -> acc ++ [term]
        False -> acc ++ []
    Syntax.Abstr term' ->
      let body = S.bodyOfAbstr term'
      in findFreeVars acc body
    Syntax.Appl term' ->
      let func = S.funcTermOfAppl term'
          arg = S.argTermOfAppl term'
      in (findFreeVars acc func) ++ (findFreeVars acc arg)
    Syntax.Base _ -> acc
    Syntax.Option term' ->
      case O.selectionOf term' of
        O.None -> acc
        O.Precisely body -> findFreeVars acc body
    Syntax.Record term' ->
      let getFree field =
            let value = R.valueOfField field
            in findFreeVars acc value 
          fields = R.fieldsOfRecordTerm term'
          vars = map getFree fields
       in concat vars

{- | Find all free variables in a term. -}
freeVars :: Syntax.Term -> [Syntax.Term]
freeVars term =
  let indexed_term = DeBruijn.indices term
  in findFreeVars [] indexed_term

{- | Errors that can happen while closing a term. -}
data Error =
    UndeclaredVar Syntax.Context Syntax.Term

instance Show Error where
  show (UndeclaredVar ctx term) =
    "Closure error: '" ++ (show term) ++ "' " ++
    "not declared in context '" ++ (show ctx) ++ "'."

{- | Results of the attempt to close a term. -}
data Result =
    Err Error
  | Ok Syntax.Term

instance Show Result where
  show (Err e) = "Error. " ++ (show e)
  show (Ok term) = "Ok. " ++ (show term)

{- | Recursively build an abstraction for each given variable. -}
abstract :: [Syntax.Term] -> Syntax.Context -> Syntax.Term -> Result
abstract [] ctx term = Ok $ term
abstract (var:vars) ctx term =
  case var of
    Syntax.Var term' ->
      let name = S.nameOfVarTerm term'
          binding = Syntax.findCtxBinding ctx name
      in case binding of
        Nothing ->
          Err $ UndeclaredVar ctx var
        Just bind ->
          let term'' = Syntax.Abstr $ S.mkAbstrTerm name bind term
          in abstract vars ctx term''

{- | Close a term by abstracting all free variables. -}
close :: Syntax.Context -> Syntax.Term -> Result
close ctx term =
  let free = freeVars term
  in abstract free ctx term
