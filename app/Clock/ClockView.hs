{-# LANGUAGE TemplateHaskell #-}

module Clock.ClockView 
  ( mountClock
  ) where

import Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import Reactive.Banana.Frameworks (MomentIO)
import Clock.ClockViewModel (ClockViewModel)
import CuteBanana.TH (bindView)

mountClock :: ClockViewModel -> MomentIO QWidget
mountClock = $(bindView "views/Clock.xml" ''ClockViewModel)
