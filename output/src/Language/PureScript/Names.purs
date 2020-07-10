-- File auto generated by purescript-bridge! --
module Language.PureScript.Names where

import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (String)

import Prelude

newtype ModuleName
  = ModuleName String


derive instance genericModuleName :: Generic ModuleName _
derive instance newtypeModuleName :: Newtype ModuleName _
--------------------------------------------------------------------------------
_ModuleName :: Iso' ModuleName String
_ModuleName = _Newtype
--------------------------------------------------------------------------------
