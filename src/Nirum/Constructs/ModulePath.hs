{-# LANGUAGE BangPatterns, NamedFieldPuns, TypeFamilies #-}
module Nirum.Constructs.ModulePath ( ModulePath ( ModuleName
                                                , ModulePath
                                                , moduleName
                                                , path
                                                )
                                   , fromFilePath
                                   , fromIdentifiers
                                   , length
                                   , hierarchy
                                   , hierarchies
                                   , replacePrefix
                                   ) where

import Prelude hiding (length)

import Data.Char (toLower)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Exts (IsList (Item, fromList, toList))

import qualified Data.Set as S
import Data.Text (intercalate, pack)
import System.FilePath (splitDirectories, stripExtension)

import Nirum.Constructs (Construct (toCode))
import Nirum.Constructs.Identifier (Identifier, fromText)

data ModulePath = ModulePath { path :: ModulePath
                             , moduleName :: Identifier
                             }
                | ModuleName { moduleName :: Identifier }
                deriving (Eq, Show)

instance Ord ModulePath where
    a <= b = toList a <= toList b

instance Construct ModulePath where
    toCode = intercalate "." . map toCode . toList

fromIdentifiers :: [Identifier] -> Maybe ModulePath
fromIdentifiers [] = Nothing
fromIdentifiers [identifier] = Just $ ModuleName identifier
fromIdentifiers identifiers = fmap (`ModulePath` last identifiers) init'
  where
    init' :: Maybe ModulePath
    init' = fromIdentifiers $ init identifiers

fromFilePath :: FilePath -> Maybe ModulePath
fromFilePath filePath =
    if F.length fileIdentifiers == F.length paths
    then fromIdentifiers fileIdentifiers
    else Nothing
  where
    replaceLast :: (a -> a) -> [a] -> [a]
    replaceLast _ [] = []
    replaceLast f l = init l ++ [f $ last l]
    paths :: [FilePath]
    paths = replaceLast (fromMaybe "" . stripExtension "nrm" . map toLower)
                        (splitDirectories filePath)
    fileIdentifiers :: [Identifier]
    fileIdentifiers = mapMaybe (fromText . pack) paths

hierarchy :: ModulePath -> S.Set ModulePath
hierarchy m@ModuleName {} = S.singleton m
hierarchy m@(ModulePath parent _) = m `S.insert` hierarchy parent

hierarchies :: S.Set ModulePath -> S.Set ModulePath
hierarchies modulePaths = S.unions $ toList $ S.map hierarchy modulePaths

length :: ModulePath -> Int
length a = length' 1 a
  where
    length' !n ModuleName { } = n
    length' !n ModulePath { path } = length' (n + 1) path

replacePrefix :: ModulePath -> ModulePath -> ModulePath -> ModulePath
replacePrefix from to path'
 | path' == from = to
 | otherwise = case path' of
                   ModuleName {} -> path'
                   ModulePath p n -> ModulePath (replacePrefix from to p) n


instance IsList ModulePath where
    type Item ModulePath = Identifier
    fromList identifiers =
        fromMaybe (error "ModulePath cannot be empty")
                  (fromIdentifiers identifiers)
    toList (ModuleName identifier) = [identifier]
    toList (ModulePath path' identifier) = toList path' ++ [identifier]
