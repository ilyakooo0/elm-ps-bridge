module Elm (makeElmModuleFromETypeDef) where

import Data.List
import Elm.Json
import Elm.Module hiding (makeElmModuleWithVersion, makeModuleContent, makeModuleContentWithAlterations)
import Elm.TyRender
import Elm.TyRep
import Elm.Versions
import Prelude

-- | Creates an Elm module for the given version. This will use the default
-- type conversion rules (to -- convert @Vector@ to @List@, @HashMap a b@
-- to @List (a,b)@, etc.).
makeElmModuleWithVersion ::
  ElmVersion ->
  -- | Module name
  String ->
  -- | List of definitions to be included in the module
  [ETypeDef] ->
  String
makeElmModuleWithVersion elmVersion moduleName defs =
  unlines
    [ moduleHeader elmVersion moduleName,
      "",
      "import Json.Decode",
      "import Json.Encode exposing (Value)",
      "-- The following module comes from bartavelle/json-helpers",
      "import Json.Helpers exposing (..)",
      "import Dict exposing (Dict)",
      "import Set exposing (Set)",
      "",
      ""
    ]
    ++ makeModuleContent defs

-- | Creates an Elm module. This will use the default type conversion rules (to
-- convert @Vector@ to @List@, @HashMap a b@ to @List (a,b)@, etc.).
--
-- default to 0.19
makeElmModuleFromETypeDef ::
  -- | Module name
  String ->
  -- | List of definitions to be included in the module
  [ETypeDef] ->
  String
makeElmModuleFromETypeDef = makeElmModuleWithVersion Elm0p19

-- | Generates the content of a module. You will be responsible for
-- including the required Elm headers. This uses the default type
-- conversion rules.
makeModuleContent :: [ETypeDef] -> String
makeModuleContent = makeModuleContentWithAlterations defaultAlterations

-- | Generates the content of a module, using custom type conversion rules.
makeModuleContentWithAlterations :: (ETypeDef -> ETypeDef) -> [ETypeDef] -> String
makeModuleContentWithAlterations alt = intercalate "\n\n" . map mkDef
  where
    mkDef typeDef =
      let def = alt typeDef
       in renderElm def ++ "\n" ++ jsonParserForDef def ++ "\n" ++ jsonSerForDef def ++ "\n"
