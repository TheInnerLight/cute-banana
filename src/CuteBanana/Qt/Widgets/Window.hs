{-# LANGUAGE TemplateHaskell #-}

module CuteBanana.Qt.Widgets.Window
  ( Window(..)
  , widget
  , build
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Reactive.Banana (Behavior)
import Reactive.Banana.Frameworks (MomentIO, liftIO)
import CuteBanana.Qt.Widget
import CuteBanana.ViewModel (sink)

data Window = Window
  { windowId       :: Maybe Text
  , windowTitle    :: Behavior Text
  , windowChildren :: [QWidget]
  }

build :: Window -> MomentIO QWidget
build w = do
  win <- liftIO $ do
    win <- QWidget.new
    setId win (windowId w)
    layout <- QVBoxLayout.new
    QBoxLayout.setSpacing layout 8
    mapM_ (QBoxLayout.addWidget layout) (windowChildren w)
    QWidget.setLayout win layout
    pure win
  sink (windowTitle w) (QWidget.setWindowTitle win . T.unpack)
  pure win

widget :: WidgetDef
widget = WidgetDef ''Window 'build
