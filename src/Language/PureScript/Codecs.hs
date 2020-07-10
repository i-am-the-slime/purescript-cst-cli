{-# LANGUAGE StandaloneDeriving #-}
module Language.PureScript.Codecs where

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

deriving instance Generic AST.Binder
instance ToJSON AST.Binder where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.TypeSearch
instance ToJSON AST.TypeSearch where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.ErrorMessageHint
instance ToJSON AST.ErrorMessageHint where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.HintCategory
instance ToJSON AST.HintCategory where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic Module
instance ToJSON Module where
  toEncoding = genericToEncoding defaultOptions


deriving instance Generic AST.RoleDeclarationData
instance ToJSON AST.RoleDeclarationData where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.TypeDeclarationData
instance ToJSON AST.TypeDeclarationData where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic (AST.ValueDeclarationData a)
instance ToJSON a => ToJSON (AST.ValueDeclarationData a) where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.DataConstructorDeclaration
instance ToJSON AST.DataConstructorDeclaration where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic (Declaration)
instance ToJSON Declaration where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.ValueFixity
instance ToJSON AST.ValueFixity where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.TypeFixity
instance ToJSON AST.TypeFixity where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.TypeInstanceBody
instance ToJSON AST.TypeInstanceBody where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.KindSignatureFor
instance ToJSON AST.KindSignatureFor where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.Guard
instance ToJSON AST.Guard where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.GuardedExpr
instance ToJSON AST.GuardedExpr where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.Expr
instance ToJSON AST.Expr where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.WhereProvenance
instance ToJSON AST.WhereProvenance where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.CaseAlternative
instance ToJSON AST.CaseAlternative where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic AST.DoNotationElement
instance ToJSON AST.DoNotationElement where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic (AST.PathTree a)
instance ToJSON a => ToJSON (AST.PathTree a) where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic (AST.PathNode a)
instance ToJSON a => ToJSON (AST.PathNode a) where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic (AST.AssocList a b)
instance (ToJSON a, ToJSON b) => ToJSON (AST.AssocList a b) where
  toEncoding = genericToEncoding defaultOptions

deriving instance Generic ((AST.Literal) a)
instance ToJSON a => ToJSON (AST.Literal a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AST.Fixity where
  parseJSON = withObject "fixity"
    $ \o -> AST.Fixity <$> o .: "associativity" <*> o .: "precedence"

instance ToJSON Env.TypeClassData where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Env.NameVisibility where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Env.NameKind where
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey (Maybe Names.ModuleName)
instance ToJSONKey (Names.Qualified (Names.ProperName Names.ClassName))
instance ToJSONKey (Names.Qualified (Names.ProperName Names.ConstructorName))
instance ToJSONKey (Names.Qualified (Names.ProperName Names.TypeName))
instance ToJSONKey (Names.Qualified Names.Ident)

instance ToJSON Env.Environment where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Env.TypeKind where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON a => ToJSON (TypeClassDictionaryInScope a) where
  toEncoding = genericToEncoding defaultOptions
