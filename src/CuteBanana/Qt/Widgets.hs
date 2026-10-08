module CuteBanana.Qt.Widgets 
  (widgets
  ) where

import CuteBanana.Qt.Widget (WidgetDef)
import qualified CuteBanana.Qt.Widgets.Button as Button
import qualified CuteBanana.Qt.Widgets.Label as Label
import qualified CuteBanana.Qt.Widgets.Window as Window

widgets :: [WidgetDef]
widgets =
  [ Window.widget
  , Label.widget
  , Button.widget
  ]
