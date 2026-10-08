{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clock.ClockViewModel
  ( ClockViewModel(..)
  , createClockViewModel
  )
  where

import Data.Text (Text)
import Reactive.Banana
import CuteBanana.ViewModel( Command, newCommand, Command(commandFired) )
import Reactive.Banana.Frameworks (MomentIO)
import CuteBanana.Model (Model(..))
import Clock.ClockModel (ClockModel(..))
import Data.Time ( ZonedTime, utcToZonedTime, defaultTimeLocale )
import Data.Time.Format (formatTime)
import qualified Data.Text as T

data ClockViewModel = ClockViewModel
  { clockViewModelTime          :: Behavior Text
  , clockViewModelDate          :: Behavior Text
  , clockViewModelFormatLabel   :: Behavior Text
  , clockViewModelToggleFormat  :: Command
  , clockViewModelSecondsLabel  :: Behavior Text
  , clockViewModelToggleSeconds :: Command
  }

createClockViewModel :: Model ClockModel -> MomentIO ClockViewModel
createClockViewModel model = do
  let m = modelValue model
  formatCmd <- newCommand (pure True)
  secondsCmd <- newCommand (pure True)
  use24h      <- accumB True (not <$ commandFired formatCmd)
  showSeconds <- accumB True (not <$ commandFired secondsCmd)
  pure ClockViewModel
    { clockViewModelTime          = formatClock <$> m <*> use24h <*> showSeconds
    , clockViewModelDate          = formatDate <$> m
    , clockViewModelFormatLabel   = (\isUse24h -> if isUse24h then "Use 12-hour" else "Use 24-hour") <$> use24h
    , clockViewModelToggleFormat  = formatCmd
    , clockViewModelSecondsLabel  = (\secondsShown -> if secondsShown then "Hide seconds" else "Show seconds") <$> showSeconds
    , clockViewModelToggleSeconds = secondsCmd
    }

formatClock :: ClockModel -> Bool  -> Bool -> Text
formatClock c use24h showSeconds = T.pack $ formatTime defaultTimeLocale pattern (localTime c)
  where
    pattern = case (use24h, showSeconds) of
      (True,  True)  -> "%H:%M:%S"
      (True,  False) -> "%H:%M"
      (False, True)  -> "%-I:%M:%S %p"
      (False, False) -> "%-I:%M %p"

formatDate :: ClockModel -> Text
formatDate c = T.pack $ formatTime defaultTimeLocale "%A, %-d %B %Y" (localTime c)

localTime :: ClockModel -> ZonedTime
localTime c = utcToZonedTime (clockZone c) (clockTime c)
