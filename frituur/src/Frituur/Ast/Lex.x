{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Frituur.Ast.Lex where

import Frituur.Location (Location (..))
import Frituur.Name (Identifier (..))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
}

%wrapper "posn-bytestring"

:-
  [\ \t\v\f\r\n]+       ;
  "//"$                 ;
  "//"[^\/].*$          ;

  "end"                 { \p _ -> EndToken       (mkL p) }
  "field"               { \p _ -> FieldToken     (mkL p) }
  "forall"              { \p _ -> ForallToken    (mkL p) }
  "hazard"              { \p _ -> HazardToken    (mkL p) }
  "in"                  { \p _ -> InToken        (mkL p) }
  "intrinsic"           { \p _ -> IntrinsicToken (mkL p) }
  "is"                  { \p _ -> IsToken        (mkL p) }
  "lambda"              { \p _ -> LambdaToken    (mkL p) }
  "of"                  { \p _ -> OfToken        (mkL p) }
  "record"              { \p _ -> RecordToken    (mkL p) }
  "risk"                { \p _ -> RiskToken      (mkL p) }
  "value"               { \p _ -> ValueToken     (mkL p) }

  "("                   { \p _ -> LeftParenthesisToken       (mkL p) }
  ")"                   { \p _ -> RightParenthesisToken      (mkL p) }
  "->"                  { \p _ -> HyphenGreaterThanToken     (mkL p) }
  "-{"                  { \p _ -> HyphenLeftBraceToken       (mkL p) }
  "."                   { \p _ -> PeriodToken                (mkL p) }
  ";"                   { \p _ -> SemicolonToken             (mkL p) }
  "{"                   { \p _ -> LeftBraceToken             (mkL p) }
  "}"                   { \p _ -> RightBraceToken            (mkL p) }
  "}>"                  { \p _ -> RightBraceGreaterThanToken (mkL p) }

  "///"$                { \p _ -> DocumentationToken (mkL p, "") }
  "/// ".*$             { \p s -> DocumentationToken (mkL p, mkT (BSL.drop 4 s)) }
  [A-Za-z][A-Za-z0-9]*  { \p s -> IdentifierToken (mkL p, mkI s) }

  "utf8"\"[^\"]*\"      { \p s -> Utf8LiteralToken (mkL p, T.dropEnd 1 (mkT (BSL.drop 5 s))) }

{
data Token
  = EndToken Location
  | FieldToken Location
  | ForallToken Location
  | HazardToken Location
  | InToken Location
  | IntrinsicToken Location
  | IsToken Location
  | LambdaToken Location
  | OfToken Location
  | RecordToken Location
  | RiskToken Location
  | ValueToken Location

  | HyphenGreaterThanToken Location
  | HyphenLeftBraceToken Location
  | LeftBraceToken Location
  | LeftParenthesisToken Location
  | PeriodToken Location
  | RightBraceGreaterThanToken Location
  | RightBraceToken Location
  | RightParenthesisToken Location
  | SemicolonToken Location

  | DocumentationToken (Location, T.Text)
  | IdentifierToken (Location, Identifier)

  | Utf8LiteralToken (Location, T.Text)

  deriving stock (Eq, Show)

mkL :: AlexPosn -> Location
mkL (AlexPn _ a b) = Location a b

mkT :: BSL.ByteString -> T.Text
mkT = TL.toStrict . TLE.decodeUtf8

mkI :: BSL.ByteString -> Identifier
mkI = Identifier . mkT
}
