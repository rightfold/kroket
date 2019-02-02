{
module Frituur.Ast.Parse where

import Control.Lens ((&), (%~), _2, view)
import Data.Foldable (toList)
import Data.Maybe (fromJust)

import qualified Data.HashSet as HS
import qualified Data.Text as T

import Frituur.Ast

import Frituur.Ast.Lex (AlexPosn (..), Token (..))
import Frituur.Intrinsic (Intrinsic)
import Frituur.Literal (Literal (..))
import Frituur.Location (Location (..))
import Frituur.Name (Identifier (..))

import qualified Frituur.Intrinsic as Intrinsic
}

%name parse
%tokentype { Token }

--------------------------------------------------------------------------------
-- Tokens

%token
  forall                    { ForallToken    $$ }
  hazard                    { HazardToken    $$ }
  in                        { InToken        $$ }
  intrinsic                 { IntrinsicToken $$ }
  is                        { IsToken        $$ }
  lambda                    { LambdaToken    $$ }
  of                        { OfToken        $$ }
  risk                      { RiskToken      $$ }
  value                     { ValueToken     $$ }

  hyphenGreaterThan         { HyphenGreaterThanToken     $$ }
  hyphenLeftBrace           { HyphenLeftBraceToken       $$ }
  leftBrace                 { LeftBraceToken             $$ }
  leftParenthesis           { LeftParenthesisToken       $$ }
  period                    { PeriodToken                $$ }
  rightBrace                { RightBraceToken            $$ }
  rightBraceGreaterThan     { RightBraceGreaterThanToken $$ }
  rightParenthesis          { RightParenthesisToken      $$ }
  semicolon                 { SemicolonToken             $$ }

  documentation             { DocumentationToken $$ }
  identifier                { IdentifierToken $$ }

  utf8Literal               { Utf8LiteralToken $$ }

%%

--------------------------------------------------------------------------------
-- Definitions

Definitions
  : { [] }
  | Definition Definitions { $1 : $2 }

Definition
  : Documentation
    hazard identifier semicolon
    { HazardDefinition (mkA $2 $1) (snd $3) }

  | Documentation
    value identifier
      of Type
      is Term semicolon
    { ValueDefinition (mkA $2 $1) (snd $3) $5 $7 }

--------------------------------------------------------------------------------
-- Types

Type
  : Type3 { $1 }

Type3
  : Type2 { $1 }
  | Type2 hyphenGreaterThan Type3
      { let functionType = FunctionType (mkA $2 []) HS.empty in
        ApplyType (unA $1) (ApplyType (mkA $2 []) functionType $1) $3 }
  | Type2 hyphenLeftBrace identifier rightBraceGreaterThan Type3
      { let functionType = FunctionType (mkA $2 []) (HS.singleton (snd $3)) in
        ApplyType (unA $1) (ApplyType (mkA $2 []) functionType $1) $5 }

Type2
  : Type1 { $1 }
  | Type2 Type1 { ApplyType (unA $1) $1 $2 }

Type1
  : forall identifier in Type { ForallType (mkA $1 []) (snd $2) $4 }
  | identifier { VariableType (mkA (fst $1) []) (snd $1) }
  | leftParenthesis Type rightParenthesis { $2 }

--------------------------------------------------------------------------------
-- Terms

Term
  : Term3 { $1 }

Term3
  : Term2 { $1 }
  | Term3 Term2 { ApplyTerm (unA $1) $1 $2 }

Term2
  : Term1 { $1 }
  | Term2 period identifier { GetRecordFieldTerm (mkA $2 []) $1 (snd $3) }

Term1
  : identifier { VariableTerm (mkA (fst $1) []) (snd $1) }
  | intrinsic identifier { IntrinsicTerm (mkA $1 []) (mkI (snd $2)) }
  | lambda identifier in Term { LambdaTerm (mkA $1 []) (snd $2) ($4) }
  | risk leftBrace identifier rightBrace Term
      { RiskTerm (mkA $1 []) (HS.singleton (snd $3)) $5 }
  | leftParenthesis Term rightParenthesis { $2 }
  | Literal { LiteralTerm (mkA (fst $1) []) (snd $1) }

--------------------------------------------------------------------------------
-- Miscellaneous

Documentation
  : { [] }
  | documentation Documentation { (snd $1) : $2 }

Literal
  : utf8Literal { $1 & _2 %~ Utf8Literal }

{
mkA :: Location -> [T.Text] -> Annotation
mkA a b = Annotation a (T.unlines b)

unA :: HasAnnotation s s a a => s -> a
unA = view annotation

mkI :: Identifier -> Intrinsic
mkI = fromJust . Intrinsic.fromName

happyError :: [Token] -> a
happyError = error . show
}
