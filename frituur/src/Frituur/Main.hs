module Frituur.Main where

import Control.Lens (Prism', (^?), prism', to)
import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.Environment (getArgs)

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import qualified Dhall

import Frituur.Anf.Translate (translateDefinition)
import Frituur.Ast.Lex (alexScanTokens)
import Frituur.Ast.Parse (parse)
import Frituur.Target.Link (linkObjects)
import Frituur.Target.Object (makeObject)
import Frituur.Target.Php.Link (linkObjectCodes)
import Frituur.Target.Php.Translate (translateAnfDefinitions)

data Options =
  Options
    { target  :: Target
    , command :: Command }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Target
  = Native
  | Php
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Interpret)

data Command
  = Translate { objectPath :: FilePath, sourcePaths :: [FilePath] }
  | Link { executablePath :: FilePath, objectPaths :: [FilePath] }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Interpret)

main :: IO ()
main = do
  args <- getArgs
  options' <- maybe usage pure $
                args ^? singleton . to T.pack
  options <- Dhall.input Dhall.auto options'

  print (options :: Options)

  case command options of
    Translate cmdObjectPath cmdSourcePaths ->
      translate cmdObjectPath cmdSourcePaths
    Link cmdExecutablePath cmdObjectPaths ->
      link cmdExecutablePath cmdObjectPaths

usage :: IO a
usage = fail "Usage: frituur CONFIGURATION"

translate :: FilePath -> [FilePath] -> IO ()
translate cmdObjectPath cmdSourcePaths = do

  -- Parse each source file.
  asts <- for cmdSourcePaths $ \cmdSourcePath -> do
    source <- BSL.readFile cmdSourcePath
    let tokens = alexScanTokens source
    pure $ parse tokens

  -- Construct a translation unit from the ASTs.
  let translationUnit = fold asts

  -- Translate the ASTs into ANF.
  anf <- fold <$> traverse translateDefinition translationUnit

  -- Translate ANF into the target language.
  let object = makeObject translateAnfDefinitions anf

  -- Emit the object file.
  BSL.writeFile cmdObjectPath $
    AE.encode object

link :: FilePath -> [FilePath] -> IO ()
link cmdExecutablePath cmdObjectPaths = do
  objects <- for cmdObjectPaths $ \cmdObjectPath -> do
    object <- BSL.readFile cmdObjectPath
    pure . fromJust $ AE.decode object
  TL.writeFile cmdExecutablePath . TB.toLazyText $
    either (error . show) id $ linkObjects linkObjectCodes objects

singleton :: Prism' [a] a
singleton = prism' pure (\case { [a] -> Just a; _ -> Nothing })
