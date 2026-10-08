{-# LANGUAGE TemplateHaskell #-}

module CuteBanana.Qt.Widgets.Button
  ( Button(..)
  , widget
  , build
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Reactive.Banana (Behavior)
import Reactive.Banana.Frameworks (MomentIO, liftIO)
import CuteBanana.Qt.Widget
import CuteBanana.ViewModel (sink, Command (..))
import Graphics.UI.Qtah.Signal (connect_)

data Button = Button
  { buttonId      :: Maybe Text
  , buttonLabel   :: Behavior Text
  , buttonEnabled :: Behavior Bool
  , buttonCommand :: Command
  }

build :: Button -> MomentIO QWidget
build w = do
  b <- liftIO QPushButton.new
  liftIO $ setId b (buttonId w)
  liftIO $ connect_ b QAbstractButton.clickedSignal $ \_checked -> runCommand (buttonCommand w)
  sink (buttonLabel w) $ QAbstractButton.setText b . T.unpack
  sink ((&&) <$> buttonEnabled w <*> commandEnabled (buttonCommand w)) $ QWidget.setEnabled b
  pure (QWidget.toQWidget b)

widget :: WidgetDef
widget = WidgetDef ''Button 'build
