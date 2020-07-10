-- File auto generated by purescript-bridge! --
module Language.PureScript.Comments where

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

data Comment
  = LineComment String
  | BlockComment String


derive instance genericComment :: Generic Comment _
--------------------------------------------------------------------------------
_LineComment :: Prism' Comment String
_LineComment = prism' LineComment f
  where
    f (LineComment a) = Just $ a
    f _ = Nothing

_BlockComment :: Prism' Comment String
_BlockComment = prism' BlockComment f
  where
    f (BlockComment a) = Just $ a
    f _ = Nothing
--------------------------------------------------------------------------------