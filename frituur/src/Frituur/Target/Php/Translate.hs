module Frituur.Target.Php.Translate
  ( translateAnfDefinitions
  , translateAnfDefinition
  , translateAnfProgram
  , translateAnfExpression
  , translateAnfValue
  ) where

import Data.Foldable (fold)
import Data.List (intersperse)

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import Frituur.Anf

import Frituur.Intrinsic (Intrinsic (..))
import Frituur.Literal (Literal (..))
import Frituur.Name (Identifier (..))

--------------------------------------------------------------------------------
-- translateAnfDefinitions

translateAnfDefinitions :: Foldable f => f (AnfDefinition a) -> T.Text
translateAnfDefinitions =
  TL.toStrict . TB.toLazyText .
    foldMap translateAnfDefinition

--------------------------------------------------------------------------------
-- translateAnfDefinition

translateAnfDefinition :: AnfDefinition a -> TB.Builder
translateAnfDefinition (AnfDefinition _ name body) =
  let cache = "$" <> kr "Cache" in
  "function " <> kg name <> "() {\n" <>
    "static " <> cache <> " = NULL;\n" <>
    "if (" <> cache <> " !== NULL) return " <> cache <> ";\n" <>
    translateAnfProgram cache body <>
    "return " <> cache <> ";\n" <>
  "}\n"

--------------------------------------------------------------------------------
-- translateAnfProgram

translateAnfProgram :: TB.Builder -> AnfProgram a -> TB.Builder
translateAnfProgram resultVariable (AnfProgram bindings result) =
  foldMap translateBinding bindings <>
  resultVariable <> " = " <> translateAnfValue result <> ";\n"
  where
  translateBinding :: (Temporary, AnfExpression a) -> TB.Builder
  translateBinding (temporary, expression) =
    translateAnfExpression ("$" <> kt temporary) expression

--------------------------------------------------------------------------------
-- translateAnfExpression

translateAnfExpression :: TB.Builder -> AnfExpression a -> TB.Builder

translateAnfExpression resultVariable (AnfApply _ left right) =
  let left'  = translateAnfValue left  in
  let right' = translateAnfValue right in
  resultVariable <> " = " <> left' <> "(" <> right' <> ");\n"

translateAnfExpression resultVariable (AnfGetRecordField _ record field) =
  let record' = translateAnfValue record in
  resultVariable <> " = " <> record' <> "['" <> kf field <> "'];\n"

translateAnfExpression resultVariable expression@(AnfLambda _ parameter body) =
  let result = "$" <> kr "Result" in
  let use = mkUse (HS.toList (anfFreeLocals expression)) in
  resultVariable <> " = function($" <> kt parameter <> ")" <> use <> " {\n" <>
    translateAnfProgram result body <>
    "return " <> result <> ";\n" <>
  "};\n"
  where
  mkUse [] = ""
  mkUse xs = " use(" <> fold (intersperse ", " xs') <> ")"
               where xs' = fmap (mappend "$" . kt) xs

translateAnfExpression resultVariable (AnfMakeRecord _ fields) =
  resultVariable <> " = [\n" <>
    foldMap translateField fields <>
  "];\n"
  where
  translateField (name, expression) =
    "'" <> kf name <> "' => " <>
      translateAnfValue expression <> ",\n"

--------------------------------------------------------------------------------
-- translateAnfValue

translateAnfValue :: AnfValue a -> TB.Builder
translateAnfValue (AnfLocal _ name) = "$" <> kt name
translateAnfValue (AnfGlobal _ name) = kg name <> "()"
translateAnfValue (AnfIntrinsic _ intrinsic) = translateIntrinsic intrinsic
translateAnfValue (AnfLiteral _ literal) = translateLiteral literal

translateIntrinsic :: Intrinsic -> TB.Builder
translateIntrinsic Coerce# = "'" <> kr "Coerce" <> "'"
translateIntrinsic Panic# = "'" <> kr "Panic" <> "'"

translateLiteral :: Literal -> TB.Builder
translateLiteral (Utf8Literal text) = "'" <> TB.fromText text <> "'"

--------------------------------------------------------------------------------
-- Names

-- |
-- Mangle a record field name.
kf :: Identifier -> TB.Builder
kf name = "kf" <> TB.fromText (identifierName name)

-- |
-- Mangle a global name.
kg :: Identifier -> TB.Builder
kg name = "kg" <> TB.fromText (identifierName name)

-- |
-- Mangle a temporary name.
kt :: Temporary -> TB.Builder
kt name = "kt" <> TB.fromString (show (temporaryId name))

-- |
-- Mangle a runtime name.
kr :: T.Text -> TB.Builder
kr name = "kr" <> TB.fromText name
