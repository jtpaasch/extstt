module Calculus.Types.Record (
    Name
  , Label
  , mkLabel
  , nameOfLabel
  , typeOfLabel
  , Field
  , mkField
  , valueOfField
  , labelOfField
  , RecordType
  , mkRecordType
  , labelsOfRecordType
  , RecordTerm
  , mkRecordTerm
  , fieldsOfRecordTerm
  , typeOfRecordTerm
  ) where

{-

| This module provides record types.

A record type consists of a set of labels, each of which
has a name and a type. To create a record type, first create
each label, then combine them into a record type.

A record consists of a set of fields, each of which
assigns a value to a label. To create a record, first
create each field, then combine them into a record.

Suppose 'bool_type' is a boolean type:

>>> bool_type
Bool

Suppose also that 'wknd_type' is a custom base type:

>>> wknd_type
Weekend

Suppose that 'true_term' is term of 'bool_type', and 'sun_term' (Sunday)
is a term of 'wknd_type'.

To create a record type, first create some labels.
Labels have a name and a type. For example:

>>> let day = mkLabel "day" wknd_type
>>> let gym = mkLabel "gym" bool_type

Now, create a record from those labels:

>>> let sunScheduleT = mkRecordType [day, gym]
>>> sunScheduleT
{day : Weekend, gym : Bool}

To create a record of this type, first create the fields.
Each field has a label and a value.

>>> let dayIsSun = mkField sun_term day
>>> dayIsSun
day = sun

>>> let gymIsTrue = mkField true_term gym
>>> gymIsTrue
gym = true

Now, create the record from these fields:

>>> let sunSchedule = mkRecordTerm [dayIsSun, gymIsTrue] sunScheduleT
>>> sunSchedule
{day = sun, gym = true} 

-}

import Data.List (intercalate)
import qualified Data.Set as Set

{- | For convenience. -}
type Name = String

{- | This represents a label. -}
data Label a = Label {
    labelName :: Name
  , labelBinding :: a
  } deriving (Eq, Ord)

instance Show a => Show (Label a) where
  show label = nameOfLabel label

{- | Creates a label. -}
mkLabel :: Name -> a -> Label a
mkLabel name binding = Label { labelName = name, labelBinding = binding }

{- | Gets the name of a label. -}
nameOfLabel :: Label a -> Name
nameOfLabel = labelName

{- | Gets the type of a label. -}
typeOfLabel :: Label a -> a
typeOfLabel = labelBinding

{- | This represents a field. -}
data Field b a = Field {
    fieldValue :: b
  , fieldLabel :: Label a
  } deriving (Eq, Ord)

instance (Show b, Show a) => Show (Field b a) where
  show field =
    (show $ labelOfField field) ++ " = " ++ (show $ valueOfField field)

{- | Creates a field. -}
mkField :: b -> Label a -> Field b a
mkField value label = Field { fieldValue = value, fieldLabel = label }

{- | Gets the value of a field. -}
valueOfField :: Field b a -> b
valueOfField = fieldValue

{- | Gets the label of a field. -}
labelOfField :: Field b a -> Label a
labelOfField = fieldLabel

{- | This represents a record type. -}
data RecordType a = RecordType {
    recordTypeLabels :: Set.Set (Label a)
  } deriving (Eq, Ord)

instance Show a => Show (RecordType a) where
  show binding =
    let fmtLabel label =
          (show label) ++ " : " ++ (show $ typeOfLabel label)
        labels = labelsOfRecordType binding
        prettierLabels = map fmtLabel labels
    in "{" ++ (intercalate ", " prettierLabels) ++ "}"

{- | Creates a record type. -}
mkRecordType :: Ord a => [Label a] -> RecordType a
mkRecordType labels = RecordType { recordTypeLabels = Set.fromList labels }

{- | Gets the labels of a record type. -}
labelsOfRecordType :: RecordType a -> [Label a]
labelsOfRecordType binding = Set.toList $ recordTypeLabels binding

{- | This represents a record (term). -}
data RecordTerm b a = RecordTerm {
    recordFields :: Set.Set (Field b a)
  , recordBinding :: RecordType a
  } deriving (Eq, Ord)

instance (Show b, Show a) => Show (RecordTerm b a) where
  show record =
    let fields = fieldsOfRecordTerm record
        binding = typeOfRecordTerm record
    in "{" ++ (intercalate ", " $ map show fields) ++ "}"

{- | Creates a record. -}
mkRecordTerm :: 
  (Ord b, Ord a) => [Field b a] -> RecordType a -> RecordTerm b a
mkRecordTerm fields binding =
  RecordTerm { recordFields = Set.fromList fields, recordBinding = binding }

{- | Gets the fields in a record. -}
fieldsOfRecordTerm :: RecordTerm b a -> [Field b a]
fieldsOfRecordTerm record = Set.toList $ recordFields record

{- | Gets the type of a record. -}
typeOfRecordTerm :: RecordTerm b a -> RecordType a
typeOfRecordTerm = recordBinding
