{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists, TypeFamilies #-}
module Nirum.Targets.TypeScript.Record ( compileRecord
                                       , compileRecordConstructor
                                       , compileRecordDeserialize
                                       , compileRecordSerialize
                                       ) where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint (Doc, (<>), (<+>))

import Nirum.CodeBuilder (nest, writeLine)
import qualified Nirum.Constructs.DeclarationSet as DS
import qualified Nirum.Constructs.Name as N
import Nirum.Constructs.TypeDeclaration ( Field (..)
                                        )

import Nirum.Package.Metadata ( Target )
import Nirum.Targets.TypeScript.Context ( CodeBuilder )
import Nirum.Targets.TypeScript.Util ( ToDoc ( .. )
                                     , TSType ( .. )
                                     , dot
                                     , list
                                     , methodDefinition
                                     , param
                                     , staticMethodDefinition
                                     , toAttributeName
                                     , toClassName
                                     , toFieldName
                                     )
import qualified Nirum.Targets.TypeScript.Util as U


compileRecord :: (Target t) => N.Name -> DS.DeclarationSet Field -> CodeBuilder t ()
compileRecord name' fields = do
    writeLine $ "export" <+> "class" <+> toClassName (N.facialName name') <+> P.lbrace
    nest 4 $ do
        compileRecordFields fields
        writeLine ""
        compileRecordConstructor fields
        writeLine ""
        compileRecordSerialize name' fields
        writeLine ""
        compileRecordDeserialize name' fields
    writeLine P.rbrace

compileRecordFields :: (Target t) => DS.DeclarationSet Field -> CodeBuilder t ()
compileRecordFields = mapM_ decl . DS.toList
  where
    decl :: (Target t) => Field -> CodeBuilder t ()
    decl field =
        writeLine $ toAttributeName (N.facialName $ fieldName field) <> P.colon <+> toDoc TSAny <> P.semi

compileRecordConstructor :: (Target t) => DS.DeclarationSet Field -> CodeBuilder t ()
compileRecordConstructor fields = methodDefinition "constructor" Nothing params' $
    mapM_ compileRecordInit fieldList
  where
    fieldList = DS.toList fields
    params' = [ param (N.facialName fieldName) (tsType fieldType) | Field { fieldName, fieldType } <- fieldList ]
    tsType _ = TSAny
    compileRecordInit :: (Target t) => Field -> CodeBuilder t ()
    compileRecordInit field =
        writeLine $ U.thisDot (toFieldName field) <+> P.equals <+> toFieldName field <> P.semi

compileRecordDeserialize :: (Target t) => N.Name -> DS.DeclarationSet Field -> CodeBuilder t ()
compileRecordDeserialize name fields = staticMethodDefinition "deserialize" (Just $ TSNirum name) params' $ do
    U.if' ("typeof" <+> toAttributeName value' <+> "!==" <+> P.quotes "object") $
        U.throw (P.text "DeserializeError") ([] :: [Doc])
    mapM_ compileRecordTypeCheck fieldList
    U.if' (dot (toAttributeName value') "_type" <+> U.ne <+> P.quotes (U.toBehindTypeName name)) $
        U.throw (P.text "DeserializeError") ([] :: [Doc])
    U.return' $ "new" <+> toClassName (N.facialName name) <> P.parens args'
  where
    fieldList = DS.toList fields
    value' = "value"
    params' = [param value' TSAny]
    args' = list P.comma $ map toFieldName fieldList
    values_ :: Field -> Doc
    values_ = dot (toAttributeName value') . toFieldName
    compileRecordTypeCheck :: (Target t) => Field -> CodeBuilder t ()
    compileRecordTypeCheck field = do
        -- ty <- lookupType $ fieldType field
        writeLine $ "const" <+> toFieldName field <> P.colon <+> toDoc TSAny <+> P.equals <+> values_ field <> P.semi

compileRecordSerialize :: (Target t) => N.Name -> DS.DeclarationSet Field -> CodeBuilder t ()
compileRecordSerialize name fields = methodDefinition "serialize" (Just TSAny) [] $ do
    writeLine $ "return" <+> P.lbrace
    nest 4 $ do
        writeLine $ "_type" <> P.colon <+> P.quotes (U.toBehindTypeName name) <> P.comma
        mapM_ field $ DS.toList fields
    writeLine $ P.rbrace <> P.semi
  where
    field :: (Target t) => Field -> CodeBuilder t ()
    field f = writeLine $ P.quotes (toFieldName f) <> P.colon <+> U.thisDot (toFieldName f) <> P.comma