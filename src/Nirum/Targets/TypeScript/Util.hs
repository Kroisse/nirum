module Nirum.Targets.TypeScript.Util (
    behindFieldName,
    className,
    facialFieldName,
) where

import Data.Text

import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.Identifier


className :: Name -> Text
className = toPascalCaseText . facialName

behindFieldName :: Field -> Text
behindFieldName = toSnakeCaseText . behindName . fieldName

facialFieldName :: Field -> Text
facialFieldName = toCamelCaseText . facialName . fieldName
