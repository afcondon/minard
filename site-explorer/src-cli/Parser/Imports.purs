module HalogenSpider.Parser.Imports where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import HalogenSpider.Types (ModuleInfo)

-- | Parse a PureScript file to extract module name and imports
parseModuleInfo :: String -> String -> Maybe ModuleInfo
parseModuleInfo filePath content = do
  moduleName <- extractModuleName content
  let imports = extractImports content
  pure { moduleName, filePath, imports }

-- | Extract module name from "module Foo.Bar where"
extractModuleName :: String -> Maybe String
extractModuleName content = do
  let allLines = lines content
  moduleLine <- Array.find (isModuleLine) allLines
  -- Parse: "module Foo.Bar where" or "module Foo.Bar (exports) where"
  let trimmed = String.trim moduleLine
  let afterModule = String.drop 7 trimmed  -- drop "module "
  -- Find end: either " where" or " ("
  let moduleName = takeUntilWhitespace afterModule
  if String.null moduleName then Nothing else Just moduleName

isModuleLine :: String -> Boolean
isModuleLine line =
  let trimmed = String.trim line
  in String.take 7 trimmed == "module " && not (String.take 2 trimmed == "--")

takeUntilWhitespace :: String -> String
takeUntilWhitespace s =
  let chars = String.toCodePointArray s
      isSpace cp = cp == String.codePointFromChar ' '
                || cp == String.codePointFromChar '('
                || cp == String.codePointFromChar '\t'
      taken = Array.takeWhile (not <<< isSpace) chars
  in String.fromCodePointArray taken

-- | Extract all import statements, returning module names
extractImports :: String -> Array String
extractImports content =
  let allLines = lines content
      importLines = Array.filter isImportLine allLines
  in Array.mapMaybe parseImportLine importLines

isImportLine :: String -> Boolean
isImportLine line =
  let trimmed = String.trim line
  in String.take 7 trimmed == "import " && not (String.take 2 trimmed == "--")

-- | Parse "import Foo.Bar" or "import Foo.Bar as X" or "import Foo.Bar (x, y)"
parseImportLine :: String -> Maybe String
parseImportLine line = do
  let trimmed = String.trim line
  let afterImport = String.drop 7 trimmed  -- drop "import "
  let moduleName = takeUntilWhitespace afterImport
  if String.null moduleName then Nothing else Just moduleName

-- | Alias mapping: alias -> full module name
type ImportAlias = { alias :: String, moduleName :: String }

-- | Extract import aliases from a file
-- | Pattern: "import Foo.Bar as Alias"
extractImportAliases :: String -> Array ImportAlias
extractImportAliases content =
  let allLines = lines content
      importLines = Array.filter isImportLine allLines
  in Array.mapMaybe parseImportAlias importLines

-- | Parse "import Foo.Bar as Alias" -> { alias: "Alias", moduleName: "Foo.Bar" }
parseImportAlias :: String -> Maybe ImportAlias
parseImportAlias line = do
  let trimmed = String.trim line
  -- Check if it has " as "
  let parts = String.split (Pattern " as ") trimmed
  case Array.length parts of
    2 -> do
      importPart <- Array.head parts
      aliasPart <- Array.index parts 1
      -- Extract module name from "import Foo.Bar"
      let afterImport = String.drop 7 importPart  -- drop "import "
      let moduleName = takeUntilWhitespace afterImport
      let alias = takeUntilWhitespace (String.trim aliasPart)
      if String.null moduleName || String.null alias
        then Nothing
        else Just { alias, moduleName }
    _ -> Nothing
