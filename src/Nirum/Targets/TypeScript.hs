{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Nirum.Targets.TypeScript ( TypeScript (TypeScript, packageName)
                                , compilePackage'
                                ) where

import Control.Monad
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy hiding (unpack)
import Data.Map.Strict hiding (empty)
import qualified Data.Map.Strict as MS
import Data.SemVer as SV hiding (toLazyText, metadata, version)
import Data.Text
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder
import qualified GHC.Exts as EXT
import System.FilePath
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import Text.Blaze.Renderer.Text
import Text.Heterocephalus hiding (compile)

import qualified Nirum.CodeBuilder as CB hiding (CodeBuilder)
import Nirum.Constructs.DeclarationSet
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.ModulePath
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import qualified Nirum.Package.ModuleSet as NMS
import Nirum.Constructs.Identifier
import Nirum.Package.Metadata hiding (fieldType)
import Nirum.Targets.TypeScript.Context hiding (empty)
import Nirum.Targets.TypeScript.TypeExpression
import qualified Nirum.Targets.TypeScript.Context as TC

type CompileError' = Markup

type Code = Markup

data Source = Source { sourcePackage :: Package TypeScript
                     , sourceModulePath :: ModulePath
                     } deriving (Eq, Ord, Show)

instance ToJSON (Package TypeScript) where
    toJSON package = object [ "name" .= p
                            , "description" .= d
                            , "version" .= SV.toText v
                            , "dependencies" .= dependencies
                            ]
      where
        Metadata { description = d, version = v } = metadata package
        TypeScript { packageName = p } = packageTarget package
        dependencies = object [ "runtypes" .= ( "^2.1.6" :: Text )
                              ]

instance Target TypeScript where
    type CompileResult TypeScript = Code
    type CompileError TypeScript = CompileError'
    targetName _ = "typescript"
    parseTarget table = do
        name' <- stringField "name" table
        return TypeScript { packageName = name' }
    compilePackage = compilePackage'
    showCompileError _ e =
        LT.toStrict $ Text.Blaze.Renderer.Text.renderMarkup e
    toByteString _ =
        Data.ByteString.Lazy.toStrict . Text.Blaze.Renderer.Utf8.renderMarkup


compileJSONMetadata :: (ToJSON a) => a -> Code
compileJSONMetadata =
    preEscapedToMarkup . toLazyText . encodePrettyToTextBuilder

compilePackage' :: Package TypeScript
                -> Map FilePath (Either CompileError' Code)
compilePackage' package =
    MS.fromList $
        files ++ [ ("package.json", Right $ compileJSONMetadata package)
                 , ("tsconfig.json", Right $ compileJSONMetadata tsConfig)
                 ]
  where
    tsConfig :: Value
    tsConfig = object [ "compilerOptions" .= compilerOptions ]
    compilerOptions :: Value
    compilerOptions = object [ "strict" .= True
                             , "target" .= ( "es2015" :: Text )
                             , "module" .= ( "commonjs" :: Text )
                             , "declaration" .= True
                             ]

    toTypeScriptFilename :: ModulePath -> [FilePath]
    toTypeScriptFilename mp =
      case mp of
        ModulePath { path = p, moduleName = mn } ->
          [ unpack (toSnakeCaseText i)
          | i <- EXT.toList p
          ]
          ++ [ f mn ]
        ModuleName { moduleName = mn } ->
          [ f mn ]
      where
        f name' = unpack (toSnakeCaseText name') ++ ".ts"
    toFilename :: Text -> ModulePath -> FilePath
    toFilename sourceRootDirectory mp =
        joinPath $ unpack sourceRootDirectory : toTypeScriptFilename mp
    files :: [(FilePath, Either CompileError' Code)]
    files = [ (toFilename "src" mp, compile (Source package mp))
            | (mp, _) <- NMS.toList $ modules package
            ]

compile :: Source -> Either CompileError' Code
compile Source { sourcePackage = package
               , sourceModulePath = mp' } = do
    let (_, code) = CB.runBuilder package mp' TC.empty moduleBody
    return [compileText|"use strict";

#{code}|]
  where
    moduleBody :: CodeBuilder TypeScript ()
    moduleBody = do
        td <- CB.boundTypes
        forM_ (DS.toList td) $ \t -> do
            compileTypeDeclaration t
            CB.appendCode $ text "\n"

compileTypeDeclaration :: TypeDeclaration
                       -> CodeBuilder TypeScript ()
compileTypeDeclaration TypeDeclaration { typename = n
                                       , type' = RecordType fields'
                                       } = compileRecordType n fields'
compileTypeDeclaration _ = CB.appendCode [compileText|
/* ------ has to be implemented
throw Error()
------------------------------*/
|]  -- never used

compileRecordType :: Name
                  -> DeclarationSet Field
                  -> CodeBuilder TypeScript ()
compileRecordType (Name fName _) fs = do
    fc <- mapM compileRecordField fields'
    CB.appendCode [compileText|export class #{className} {
%{ forall f <- fc }
    #{ f };
%{ endforall }

    constructor(
%{ forall f <- fc }
        #{ f },
%{ endforall }
    ) {
%{ forall f <- fields' }
        this.#{toCamelCase' f} = #{toCamelCase' f}
%{ endforall }
    }
}
|]
  where
    className :: Text
    className = toPascalCaseText fName
    fields' :: [Field]
    fields' = DS.toList fs
    toCamelCase' :: Field -> Text
    toCamelCase' Field { fieldName = (Name fieldFacialName _) } =
        toCamelCaseText fieldFacialName
    compileRecordField :: Field -> CodeBuilder TypeScript Markup
    compileRecordField fd@Field { fieldType = ft } = do
        fieldType' <- compileTypeExpression $ Just ft
        return [compileText|#{toCamelCase' fd}: #{fieldType'}|]
