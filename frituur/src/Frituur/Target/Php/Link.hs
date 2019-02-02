module Frituur.Target.Php.Link
  ( linkObjectCodes
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB

linkObjectCodes :: [T.Text] -> TB.Builder
linkObjectCodes codes =
  prologue <> foldMap TB.fromText codes <> epilogue

prologue :: TB.Builder
prologue =
  "<?php\n\
  \declare(strict_types = 1);\n\
  \function krCoerce($value) {\n\
  \  return $value;\n\
  \}\n\
  \function krPanic($message) {\n\
  \  fprint(STDERR, $message);\n\
  \  exit(1);\n\
  \}\n"

epilogue :: TB.Builder
epilogue =
  "kgMain()([\n\
  \  'kfOutput' => [\n\
  \    'kfWrite' => function($buffer) {\n\
  \      echo $buffer;\n\
  \    },\n\
  \  ],\n\
  \]);\n"
