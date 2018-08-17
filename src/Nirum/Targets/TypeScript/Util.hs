module Nirum.Targets.TypeScript.Util (
    behindFieldName,
    className,
    facialFieldName,
) where

import Data.Text

import qualified Nirum.Constructs.Declaration as D
import Nirum.Constructs.Name
import Nirum.Constructs.Identifier


className :: Name -> Text
className = toPascalCaseText . facialName

behindFieldName :: D.Declaration a => a -> Text
behindFieldName = toSnakeCaseText . behindName . D.name

facialFieldName :: D.Declaration a => a -> Text
facialFieldName = toCamelCaseText . facialName . D.name
