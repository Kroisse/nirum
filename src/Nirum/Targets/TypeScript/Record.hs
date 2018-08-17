{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.TypeScript.Record (compileRecordType) where

import Data.Text as T
import Text.Heterocephalus hiding (compile)

import qualified Nirum.CodeBuilder as CB hiding (CodeBuilder)
import Nirum.Constructs.DeclarationSet
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import Nirum.Constructs.Identifier
import {-# SOURCE #-} Nirum.Targets.TypeScript ()
import Nirum.Targets.TypeScript.Context hiding (empty)


compileRecordType :: Name
                  -> DeclarationSet Field
                  -> CodeBuilder TypeScript ()
compileRecordType (Name fName _) fs = do
    ck <- CB.render_ $ compileTypeChecker fields'
    CB.appendCode [compileText|export class #{className} {
    static _checker = #{ ck };

    constructor(
%{ forall field <- fields' }
        public readonly #{ facialFieldName field }: any,
%{ endforall }
    ) {
        Object.freeze(this);
    }

    public static fromValue(value: unknown) {
        const checked = this._checker.check(value);
        return new #{ className }(
%{ forall field <- fields' }
            checked.#{ behindFieldName field } as any,
%{ endforall }
        );
    }

    public toValue(): _r.Static<typeof #{ className }._checker> {
        return {
%{ forall field <- fields' }
            #{ behindFieldName field }: this.#{ facialFieldName field },
%{ endforall }
        };
    }
}
|]
  where
    className :: Text
    className = toPascalCaseText fName
    fields' :: [Field]
    fields' = DS.toList fs
    behindFieldName :: Field -> Text
    behindFieldName = toSnakeCaseText . behindName . fieldName
    facialFieldName :: Field -> Text
    facialFieldName = toCamelCaseText . facialName . fieldName
    compileTypeChecker :: [Field] -> CodeBuilder TypeScript ()
    compileTypeChecker fs' = CB.appendCode [compileText|_r.Record({
%{ forall field <- fs' }
        #{ behindFieldName field }: _r.Always,
%{ endforall }
    })|]
