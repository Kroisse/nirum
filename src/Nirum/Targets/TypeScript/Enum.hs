{-# LANGUAGE QuasiQuotes #-}
module Nirum.Targets.TypeScript.Enum (compileEnumType) where

import Text.Heterocephalus hiding (compile)

import qualified Nirum.CodeBuilder as CB hiding (CodeBuilder)
import Nirum.Constructs.DeclarationSet
import qualified Nirum.Constructs.DeclarationSet as DS
import Nirum.Constructs.Name
import Nirum.Constructs.TypeDeclaration
import {-# SOURCE #-} Nirum.Targets.TypeScript ()
import Nirum.Targets.TypeScript.Context hiding (empty)
import Nirum.Targets.TypeScript.Util


compileEnumType :: Name
                -> DeclarationSet EnumMember
                -> CodeBuilder TypeScript ()
compileEnumType name enumMembers = do
    let members' = DS.toList enumMembers
    let enumName = className name
    CB.appendCode [compileText|export enum #{ enumName } {
%{ forall member <- members' }
    #{ facialFieldName member } = '#{ behindFieldName member }',
%{ endforall }
}

export namespace #{ enumName } {
    export const schema = _r.Union(
%{ forall member <- members' }
        _r.Literal('#{ behindFieldName member }'),
%{ endforall }
    );

    export function fromValue(value: unknown): #{ enumName } {
        const checked = schema.check(value);
        switch (checked) {
%{ forall m <- members' }
        case '#{ behindFieldName m }':
            return #{ enumName }.#{ facialFieldName m };
%{ endforall }
        default:
            throw new Error();
        }
    }

    export function toValue(self: #{ enumName }): _r.Static<typeof schema> {
        return self;
    }
}
|]
