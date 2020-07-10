{-# LANGUAGE StandaloneDeriving #-}
module Lib
  ( parseFile
  , createPSTypes
  )
where

import           Protolude               hiding ( Fixity )
import           Data.Aeson                     ( parseJSON
                                                , ToJSON
                                                , ToJSONKey
                                                , FromJSON
                                                , toEncoding
                                                , genericToEncoding
                                                , defaultOptions
                                                , (.=)
                                                , (.:)
                                                , withObject
                                                )
import           Language.PureScript.CST        ( parseFromFile )
import           Language.PureScript.AST        ( Module(..)
                                                , Declaration(..)
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Language.PureScript.AST       as AST
import qualified Language.PureScript.Names     as Names
import qualified Language.PureScript.Environment
                                               as Env
import qualified Language.PureScript.Types     as Types
import qualified Language.PureScript.Comments  as Comments
import           Language.PureScript.TypeClassDictionaries
                                                ( TypeClassDictionaryInScope )
import           Language.PureScript.Bridge     ( writePSTypes
                                                , buildBridge
                                                , defaultBridge
                                                , SumType
                                                , Language(..)
                                                , equal
                                                , order
                                                , mkSumType
                                                )
import           Language.PureScript.Codecs

myTypes :: [SumType Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy AST.Module)
  , mkSumType (Proxy :: Proxy AST.SourcePos)
  , mkSumType (Proxy :: Proxy AST.SourceSpan)
  , mkSumType (Proxy :: Proxy AST.Declaration)
  , mkSumType (Proxy :: Proxy Comments.Comment)
  , mkSumType (Proxy :: Proxy Names.ModuleName)
  , mkSumType (Proxy :: Proxy Env.DataDeclType)
  , mkSumType (Proxy :: Proxy Env.FunctionalDependency)
  , mkSumType (Proxy :: Proxy Env.NameKind)
  , mkSumType (Proxy :: Proxy Types.Constraint)
  , mkSumType (Proxy :: Proxy Types.Type)
  , mkSumType (Proxy :: Proxy Names.ClassName)
  , mkSumType (Proxy :: Proxy Names.TypeName)
  , mkSumType (Proxy :: Proxy Names.Ident)
  , mkSumType (Proxy :: Proxy Names.ModuleName)
  , mkSumType (Proxy :: Proxy Names.ProperName)
  , mkSumType (Proxy :: Proxy Names.Qualified)
  ]
createPSTypes = writePSTypes "./output/src" (buildBridge defaultBridge) myTypes

parseFile :: Text -> IO ()
parseFile rawPureScriptCode =
  putText $ case parseFromFile "Standard Input" rawPureScriptCode of
    (_, Left e             ) -> show e
    (_, Right (r :: Module)) -> toS (encodePretty r)
