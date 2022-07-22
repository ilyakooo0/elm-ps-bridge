module Main (main) where

import Control.Monad.Writer.Strict
import Data.Aeson hiding (Options)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map.Lazy qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Yaml as Yaml
import Elm (makeElmModuleFromETypeDef)
import Elm.TyRep
import Language.PureScript.Bridge hiding (SumType)
import Language.PureScript.Bridge qualified as PS
import Language.PureScript.Bridge.CodeGenSwitches qualified as PSS
import Language.PureScript.Bridge.PSTypes
import Options.Generic
import Relude

main :: IO ()
main = do
  Options {input, elmOutput, psOutput} <- getRecord "elm-ps-bridge"
  bridge :: Map Text Datatype <- Yaml.decodeFileThrow input
  writeFile elmOutput $ elm bridge
  writeFileText psOutput $ ps bridge

data Datatype
  = SumDatatype SumType
  | ProductType (Map Text Datatype)
  | BoolType
  | IntType
  | FloatType
  | StringType
  deriving stock (Show)

newtype SumType = SumType (Map Text (Maybe Datatype))
  deriving stock (Show)

parseSumType :: Vector Value -> Parser SumType
parseSumType = fmap (SumType . fold) . traverse parseConstructor . V.toList
  where
    parseConstructor (String s) = pure $ one (s, Nothing)
    parseConstructor (Object o) | [(Key.toText -> k, v)] <- KeyMap.toList o = one . (k,) . Just <$> parseJSON v
    parseConstructor (Object _) = fail "Sum types can only have one name per constructor"
    parseConstructor _ = fail "Sum Bad sum type ;("

instance FromJSON Datatype where
  parseJSON (Array a) = SumDatatype <$> parseSumType a
  parseJSON (Object o) = ProductType <$> traverse parseJSON (M.mapKeysMonotonic Key.toText $ KeyMap.toMap o)
  parseJSON (String "Bool") = pure BoolType
  parseJSON (String "Int") = pure IntType
  parseJSON (String "Float") = pure FloatType
  parseJSON (String "String") = pure StringType
  parseJSON _ = fail "Bad datatype ;("

data Options = Options
  { input :: FilePath,
    elmOutput :: FilePath,
    psOutput :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (ParseRecord)

elm :: Map Text Datatype -> String
elm datattypes =
  let (_, types) = runWriter $ traverse (uncurry elmType) $ M.toList datattypes
   in makeElmModuleFromETypeDef "Bridge" types

elmType :: Text -> Datatype -> Writer [ETypeDef] EType
elmType name BoolType = pure $ ETyCon $ ETCon "Bool"
elmType name IntType = pure $ ETyCon $ ETCon "Int"
elmType name FloatType = pure $ ETyCon $ ETCon "Float"
elmType name StringType = pure $ ETyCon $ ETCon "String"
elmType name (ProductType fields) = do
  fields' <- traverse (\(k, v) -> (toString k,) <$> elmType (name <> "_" <> k) v) $ M.toList fields
  tell
    [ ETypeAlias
        EAlias
          { ea_name =
              ETypeName
                { et_name = toString name,
                  et_args = []
                },
            ea_fields = fields',
            ea_omit_null = False,
            ea_newtype = False,
            ea_unwrap_unary = False
          }
    ]
  pure $ ETyCon $ ETCon $ toString name
elmType name (SumDatatype (SumType sums)) = do
  sums' <- traverse (\(k, v) -> (toString k,) <$> traverse (elmType (name <> "_" <> k)) v) $ M.toList sums
  tell
    [ ETypeSum
        ESum
          { es_name =
              ETypeName
                { et_name = toString name,
                  et_args = []
                },
            es_constructors =
              sums' <&> \(name, value) ->
                STC
                  { _stcName = name,
                    _stcEncoded = name,
                    _stcFields = Anonymous $ maybeToList value
                  },
            es_omit_null = False,
            es_unary_strings = True,
            es_type = defSumEncoding
          }
    ]
  pure $ ETyCon $ ETCon $ toString name

psType :: Text -> Datatype -> Writer [PS.SumType PureScript] (PS.TypeInfo PureScript)
psType _ BoolType = pure psBool
psType _ IntType = pure psInt
psType _ FloatType = pure psNumber
psType _ StringType = pure psString
psType name (SumDatatype (SumType sums)) = do
  sums' <- traverse (\(k, v) -> (k,) <$> traverse (psType (name <> "_" <> k)) v) $ M.toList sums
  let ti =
        TypeInfo
          { _typePackage = "bridge",
            _typeModule = "Bridge",
            _typeName = name,
            _typeParameters = []
          }
  tell
    [ PS.SumType
        ti
        ( sums' <&> \(name, value) ->
            DataConstructor
              { _sigConstructor = name,
                _sigValues = Left $ maybeToList value
              }
        )
        allInstances
    ]
  pure ti
psType name (ProductType fields) = do
  fields' <- traverse (\(k, v) -> (k,) <$> psType (name <> "_" <> k) v) $ M.toList fields
  let ti =
        TypeInfo
          { _typePackage = "bridge",
            _typeModule = "Bridge",
            _typeName = name,
            _typeParameters = []
          }
  tell
    [ PS.SumType
        ti
        [ DataConstructor
            { _sigConstructor = name,
              _sigValues =
                Right $
                  fields' <&> \(name, value) ->
                    RecordEntry
                      { _recLabel = name,
                        _recValue = value
                      }
            }
        ]
        allInstances
    ]
  pure ti

allInstances :: [PS.Instance]
allInstances =
  [ PS.Encode,
    PS.Decode,
    PS.Generic,
    PS.Eq,
    PS.Ord
  ]

ps :: Map Text Datatype -> Text
ps datattypes =
  let (_, types) = runWriter $ traverse (uncurry psType) $ M.toList datattypes
   in moduleToText
        PSS.Settings
          { generateLenses = False,
            genericsGenRep = True,
            generateForeign = Nothing
          }
        PSModule
          { psModuleName = "Bridge",
            psImportLines = mempty,
            psTypes = types
          }