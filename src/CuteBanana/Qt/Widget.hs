module CuteBanana.Qt.Widget
  ( WidgetDef (..)
  , AttrSpec (..)
  , ValueType (..)
  , showValueType
  , setId
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import Graphics.UI.Qtah.Core.QObject (QObjectPtr)
import Language.Haskell.TH.Syntax (Name)

data WidgetDef = WidgetDef
  { wdType  :: Name
  , wdBuild :: Name
  }

data AttrSpec
  = DisplayAttrSpec  ValueType  -- ^ Display a value, binding to a Behaviour or a Field
  | EditableAttrSpec ValueType  -- ^ Display and/or edit a value, binding to a Field
  | ActionAttrSpec              -- ^ Invocation an action, binding to a Command
  deriving (Eq, Show)

data ValueType 
  = VText 
  | VBool
  deriving (Eq, Show)

showValueType :: ValueType -> String
showValueType VText = "Text"
showValueType VBool = "Bool"

setId :: QObjectPtr w => w -> Maybe Text -> IO ()
setId w = maybe (pure ()) (QObject.setObjectName w . T.unpack)
