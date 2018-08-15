{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeOperators #-}
-- | The 'CodeBuilder' monad.
module Nirum.CodeBuilder (
    -- * The CodeBuilder monad
    CodeBuilder,
    appendCode,
    boundTypes,
    runBuilder,
    lookupType,
    ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import qualified Control.Monad.State as ST
import Control.Monad.State (MonadState, State, state, runState)
import Data.Functor (Functor)
import Data.Maybe (fromMaybe)
import Text.Blaze

import Nirum.Constructs.DeclarationSet hiding (empty)
import Nirum.Constructs.Identifier (Identifier)
import Nirum.Constructs.ModulePath (ModulePath)
import Nirum.Constructs.TypeDeclaration
import Nirum.Package.Metadata
import qualified Nirum.TypeInstance.BoundModule as BoundModule

-- | A code builder monad parameterized by:
--
--     * @t@ - The build target
--     * @s@ - The state
newtype Target t => CodeBuilder t s a = CodeBuilder (State (BuildState t s) a)
    deriving ( Applicative
             , Functor
             , Monad
             )

data Target t => BuildState t s =
    BuildState { output :: Markup
               , boundModule :: BoundModule.BoundModule t
               , innerState :: s
               }

instance Target t => MonadState s (CodeBuilder t s) where
    state f = do
        st <- get'
        let (a, s) = f (innerState st)
        put' $ st { innerState = s }
        return a

get' :: Target t => CodeBuilder t s (BuildState t s)
get' = CodeBuilder ST.get

put' :: Target t => BuildState t s -> CodeBuilder t s ()
put' = CodeBuilder . ST.put

modify' :: Target t
        => (BuildState t s -> BuildState t s)
        -> CodeBuilder t s ()
modify' = CodeBuilder . ST.modify

appendCode :: Target t
           => Markup
           -> CodeBuilder t s ()
appendCode code = modify' $ \ s -> s { output = output s <> code }

-- | Look up the actual type by the name from the context of the builder
-- computation.
lookupType :: Target t
           => Identifier                     -- ^ name of the type to find
           -> CodeBuilder t s BoundModule.TypeLookup
lookupType identifier = do
    m <- fmap boundModule get'
    return $ BoundModule.lookupType identifier m

-- | Execute the builder computation and retrive output.
runBuilder :: Target t
           => Package t
           -> ModulePath
           -> s                  -- ^ initial state
           -> CodeBuilder t s a  -- ^ code builder computation to execute
           -> (a, Markup)     -- ^ return value and build result
runBuilder package modPath st (CodeBuilder a) = (ret, out')
  where
    mod' = fromMaybe (error "never happend")
                     (BoundModule.resolveBoundModule modPath package)
    initialState = BuildState { output = mempty
                              , boundModule = mod'
                              , innerState = st
                              }
    (ret, finalState) = runState a initialState
    out' = output finalState

boundTypes :: Target t => CodeBuilder t s (DeclarationSet TypeDeclaration)
boundTypes = do
    m <- fmap boundModule get'
    return $ BoundModule.boundTypes m
