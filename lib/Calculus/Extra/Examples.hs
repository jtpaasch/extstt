module Calculus.Extra.Examples where

{- | This module includes some examples for experimenting/debugging. -}

import qualified Calculus.Types.Simple as S
import qualified Calculus.Types.Base as B
import qualified Calculus.Types.Option as O
import qualified Calculus.Types.Record as R
import qualified Calculus.Types.List as L
import qualified Calculus.Language.Syntax as Syntax
import qualified Calculus.Language.Check as Check
import qualified Calculus.Eval.Evaluator as Eval
import qualified Calculus.Eval.Utils as Utils

-- Create a "Bool" base type, with one inhabitant ("true").
boolT = B.mkBaseType "Bool"
true = B.mkBaseTerm "true" boolT

-- Register the type and the term as part of the calculus's syntax.
bool_type = Syntax.BaseT boolT
true_term = Syntax.Base true

-- Create a "Weekend" base type, with two inhabitants ("sat" and "sun").
wkndT = B.mkBaseType "Weekend"
sat = B.mkBaseTerm "sat" wkndT
sun = B.mkBaseTerm "sun" wkndT

-- Register the type and terms as part of the calculus's syntax.
wknd_type = Syntax.BaseT wkndT
sat_term = Syntax.Base sat
sun_term = Syntax.Base sun

-- Create a context.
ctx = Syntax.mkCtx [("w", bool_type), ("x", bool_type), ("y", bool_type)]

-- Create some variables.
w = S.mkVarTerm "w"
x = S.mkVarTerm "x"
y = S.mkVarTerm "y"
z = S.mkVarTerm "z" -- Not assigned a type in context.

-- Register the terms as part of the calculus's syntax.
w_term = Syntax.Var w
x_term = Syntax.Var x
y_term = Syntax.Var y
z_term = Syntax.Var z

-- Check the types of these terms:
-- Check.getType ctx x_term
-- Check.getType ctx y_term

-- Create an abstraction.
f = S.mkAbstrTerm "x" bool_type x_term

-- Register it as a term in the calculus.
f_term = Syntax.Abstr f

-- Create some applications.
fx = S.mkApplTerm f_term x_term
fy = S.mkApplTerm f_term y_term

-- Register them as terms in the calculus.
fx_term = Syntax.Appl fx
fy_term = Syntax.Appl fy

-- Check the types of these terms
-- Check.getType ctx f_term
-- Check.getType ctx fx_term
-- Check.getType ctx fy_term

-- Evaluate some of them:
-- Eval.reduce ctx fx_term
-- Eval.reduce ctx fy_term

g = S.mkAbstrTerm "y" bool_type x_term
g_term = Syntax.Abstr g
h = S.mkAbstrTerm "x" bool_type g_term
h_term = Syntax.Abstr h
hw = S.mkApplTerm h_term w_term
hw_term = Syntax.Appl hw
hy = S.mkApplTerm h_term y_term -- No capture happens during evaluation.
hy_term = Syntax.Appl hy

-- Evaluate some of those terms:
-- Eval.reduce ctx hw_term
-- Eval.reduce ctx hy_term

-- Create an "Optional Bool" base type.
optBoolT = O.mkOptionType bool_type

-- Select no Bool term, and then select a particular Bool term.
no_optBool = O.mkOptionTerm O.None optBoolT
true_optBool = O.mkOptionTerm (O.Precisely true_term) optBoolT

-- Register the type and terms as part of the calculus's syntax.
optBool_type = Syntax.OptionT optBoolT
no_optBool_term = Syntax.Option no_optBool
true_optBool_term = Syntax.Option true_optBool

-- Check the types:
-- Check.getType ctx no_optBool_term
-- Check.getType ctx true_optBool_term

-- Select a particular variable for an option.
x_optBool = O.mkOptionTerm (O.Precisely x_term) optBoolT

-- Register it as a term in the calculus's syntax.
x_optBool_term = Syntax.Option x_optBool

-- Check it's type:
-- Check.getType ctx x_optBool_term

-- Make an an abstraction over it, and apply it to an argument.
k = S.mkAbstrTerm "x" bool_type x_optBool_term
k_term = Syntax.Abstr k
kw = S.mkApplTerm k_term w_term
kw_term = Syntax.Appl kw

-- Check it's type, and evaluate it:
-- Check.getType ctx kw_term
-- Eval.reduce ctx kw_term

-- Create some labels.
day = R.mkLabel "day" wknd_type
soccer = R.mkLabel "soccer" bool_type

-- Create a record type with the labels, and register it with the calculus.
wkndSportT = R.mkRecordType [day, soccer]
wkndSport_type = Syntax.RecordT wkndSportT

-- Create some fields.
sat_dayField = R.mkField sat_term day
true_socField = R.mkField true_term soccer

-- Create a record with the fields, and register it with the calculus.
satSoc_wkndSport = R.mkRecordTerm [sat_dayField, true_socField] wkndSportT
satSoc_wkndSport_term = Syntax.Record satSoc_wkndSport

-- Construct a record with a field of the wrong type.
sun_socField = R.mkField sun_term soccer
bad_wkndSport = R.mkRecordTerm [sat_dayField, sun_socField] wkndSportT
bad_wkndSport_term = Syntax.Record bad_wkndSport -- Doesn't type check.
-- Check.getType ctx bad_wkndSport_term

-- Construct a record with the wrong fields.
wrong_wkndSport = R.mkRecordTerm [sat_dayField] wkndSportT
wrong_wkndSport_term = Syntax.Record wrong_wkndSport -- Doesn't type check.
-- Check.getType ctx wrong_wkndSport_term

-- Construct a record with a variable in it.
x_socField = R.mkField x_term soccer
varSatSoc_wkndSport = R.mkRecordTerm [sat_dayField, x_socField] wkndSportT
varSatSoc_wkndSport_term = Syntax.Record varSatSoc_wkndSport

-- Make an abstraction with it, and apply it to an argument.
m = S.mkAbstrTerm "x" bool_type varSatSoc_wkndSport_term
m_term = Syntax.Abstr m
mw = S.mkApplTerm m_term w_term
mw_term = Syntax.Appl mw

-- Check it's type, and evaluate it:
-- Check.getType ctx mx_term
-- Eval.reduce ctx mx_term

-- Create a record with an untypable term.
z_socField = R.mkField z_term soccer -- z is not in context.
zSatSoc_wkndSport = R.mkRecordTerm [sat_dayField, z_socField] wkndSportT
zSatSoc_wkndSport_term = Syntax.Record zSatSoc_wkndSport -- Won't type check.
-- Check.getType ctx zSatSoc_wkndSport_term

-- Find all free variables in a term.
free = Utils.freeVars g_term

-- Close a term by abstracting all free variables:
closed_g = Utils.close ctx g_term

-- Create a bool list type.
boolListT = L.mkListType bool_type

-- Register it with the calculus syntax:
boolList_type = Syntax.ListT boolListT

-- Create an empty bool list, and register it with the syntax
emptyBoolList = L.mkListTerm L.Empty boolListT
emptyBoolList_term = Syntax.List emptyBoolList

-- Cons a 'true_term' on the front of it, and register it with the syntax.
cons'd_list = L.Cons true_term emptyBoolList_term
otherBoolList = L.mkListTerm cons'd_list boolListT
otherBoolList_term = Syntax.List otherBoolList

-- Cons a weekend term on the front of a bool list. 
cons'd_badList = L.Cons sun_term otherBoolList_term
badList = L.mkListTerm cons'd_badList boolListT
badList_term = Syntax.List badList -- Won't type check.
-- Check.getType ctx badList_term

-- Build a list with a variable in it.
cons'd_w_list = L.Cons w_term otherBoolList_term
w_boolList = L.mkListTerm cons'd_w_list boolListT
w_boolList_term = Syntax.List w_boolList

-- Make an abstraction with it.
r = S.mkAbstrTerm "w" bool_type w_boolList_term
r_term = Syntax.Abstr r
free_in_r = Utils.freeVars r_term
rx = S.mkApplTerm r_term x_term
rx_term = Syntax.Appl rx

-- Check it's type, and evaluate it:
-- Check.getType ctx rx_term
-- Eval.reduce ctx rx_term
