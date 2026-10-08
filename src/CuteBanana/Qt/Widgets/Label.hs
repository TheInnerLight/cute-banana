{-# LANGUAGE TemplateHaskell #-}

module CuteBanana.Qt.Widgets.Label 
  ( Label(..)
  , widget
  , build
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Reactive.Banana (Behavior)
import Reactive.Banana.Frameworks (MomentIO, liftIO)
import CuteBanana.Qt.Widget
import CuteBanana.ViewModel (sink)

data Label = Label
  { labelId :: Maybe Text
  , labelText :: Behavior Text
  , labelStyle :: Behavior Text
  }

build :: Label -> MomentIO QWidget
build w = do
  l <- liftIO (QLabel.newWithText "")
  liftIO (setId l (labelId w))
  sink (labelText w) (QLabel.setText l . T.unpack)
  sink (labelStyle w) (QWidget.setStyleSheet l . T.unpack)
  pure (QWidget.toQWidget l)

widget :: WidgetDef
widget = WidgetDef ''Label 'build
