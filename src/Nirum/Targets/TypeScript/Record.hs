{-# LANGUAGE FlexibleInstances, NamedFieldPuns, OverloadedLists,
             TypeFamilies #-}
module Nirum.Targets.TypeScript.Record ( compileRecord
                                       , compileRecordConstructor
                                       , compileRecordDeserialize
                                       , compileRecordSerialize
                                       ) where

import Text.PrettyPrint hiding (nest)

import Nirum.CodeBuilder hiding (CodeBuilder)
import Nirum.Constructs.DeclarationSet
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata hiding (fieldType)
import Nirum.Targets.TypeScript.Context
import Nirum.Targets.TypeScript.Util

compileRecord :: (Target t) => Name -> DeclarationSet Field -> CodeBuilder t ()
compileRecord name' fields = do
    let className = toClassName (facialName name')
    writeLine $ "export" <+> "class" <+> className <+> lbrace
    nest 4 $ do
        compileRecordFields fields
        writeLine ""
        compileRecordConstructor fields
        writeLine ""
        compileRecordSerialize name' fields
        writeLine ""
        compileRecordDeserialize name' fields
    writeLine rbrace

compileRecordFields :: (Target t) => DeclarationSet Field -> CodeBuilder t ()
compileRecordFields = mapM_ decl . toList
  where
    decl :: (Target t) => Field -> CodeBuilder t ()
    decl field =
        writeLine $ toFieldName field <> colon <+> toDoc TSAny <> semi

compileRecordConstructor :: Target t
                         => DeclarationSet Field
                         -> CodeBuilder t ()
compileRecordConstructor fields =
    methodDefinition "constructor" Nothing params' $
        mapM_ compileRecordInit fieldList
  where
    fieldList = toList fields
    params' = [ param (facialName fieldName) (tsType fieldType)
              | Field { fieldName, fieldType } <- fieldList
              ]
    tsType _ = TSAny
    compileRecordInit :: (Target t) => Field -> CodeBuilder t ()
    compileRecordInit field =
        writeLine $ thisDot fieldName <+> equals <+> fieldName <> semi
      where
        fieldName = toFieldName field

compileRecordDeserialize :: Target t
                         => Name
                         -> DeclarationSet Field
                         -> CodeBuilder t ()
compileRecordDeserialize name fields =
    staticMethodDefinition "deserialize" (Just $ TSNirum name) params' $ do
        if' ("typeof" <+> attr' `ne` quotes "object") $
            throw (text "DeserializeError") ([] :: [Doc])
        mapM_ compileRecordTypeCheck fieldList
        if' (dot attr' "_type" `ne` quotes (toBehindTypeName name)) $
            throw (text "DeserializeError") ([] :: [Doc])
        return' $ "new" <+> toClassName (facialName name) <> parens args'
  where
    fieldList = toList fields
    value' = "value"
    params' = [param value' TSAny]
    args' = list comma $ map toFieldName fieldList
    attr' = toAttributeName value'
    values_ :: Field -> Doc
    values_ = dot attr' . toFieldName
    compileRecordTypeCheck :: Target t => Field -> CodeBuilder t ()
    compileRecordTypeCheck field =
        constDecl (fieldName field) TSAny (values_ field)

compileRecordSerialize :: (Target t)
                       => Name
                       -> DeclarationSet Field
                       -> CodeBuilder t ()
compileRecordSerialize name fields =
    methodDefinition "serialize" (Just TSAny) [] $ do
        writeLine $ "return" <+> lbrace
        nest 4 $ do
            writeLine $ "_type" <> colon <+>
                        quotes (toBehindTypeName name) <> comma
            mapM_ field $ toList fields
        writeLine $ rbrace <> semi
  where
    field :: (Target t) => Field -> CodeBuilder t ()
    field f = writeLine $ quotes (toFieldName f) <> colon <+>
                          thisDot (toFieldName f) <> comma
