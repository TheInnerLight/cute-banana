# Cute Banana

- Do you like Haskell?
- Do you like Functional Reactive Programming?
- Do you like QT?
- Are you exhausted by the monopoly of the Elm architecture?
- Do you wish UI development were more like WPF?
- Do you wish you could write more XML?
- Are you ready to party like it's 2010?

If the answer is __yes__ to all of these things, you've arrived in the right place.

`cute-banana` is a Haskell library for implementing Model-View-View Model (MVVM) using Functional Reactive Programming with Reactive Banana, QT bindings and just enough Template Haskell to keep the doctor away.

## View

Views are markup defined in xml. They don't have any logic, they just bind to definitions in the view model.

`/views/clock.xml`
```xml
<Window title="Clock">
  <Label  id="time" text="{clockViewModelTime}" style="font-size: 48pt; font-weight: 600; qproperty-alignment: AlignCenter;"/>
  <Label  id="date" text="{clockViewModelDate}" style="font-size: 14pt; qproperty-alignment: AlignCenter;"/>
  <Button id="format"  label="{clockViewModelFormatLabel}"  command="{clockViewModelToggleFormat}"/>
  <Button id="seconds" label="{clockViewModelSecondsLabel}" command="{clockViewModelToggleSeconds}"/>
</Window>
```

In source, you just bind to the view markup definition.

`app/Clock/ClockView.hs`
```haskell 
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
```

## View Model

The view model is where the logic of the presentation of your UI lives. Note the lack of callbacks, explicit state threading, etc.

`app/Clock/ClockViewModel.hs`
```haskell
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
  use24h <- accumB True (not <$ commandFired formatCmd)
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
```

## Model

The model is your domain. It doesn't know anything about how it's going to get presented.

Why is it wrapped in STM? If your application makes arbitrary updates to the domain model that the presentation layers needs to track, this is a convenient way of exposing that. Here we have a clock model that is continually updated to the current time in a new thread and the UI can follow those changes.

`app/Clock/ClockModel.hs`

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Clock.ClockModel
  ( ClockModel(..)
  , createClockModel
  ) where

import Control.Concurrent.STM
import Data.Time (UTCTime, TimeZone)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getTimeZone)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import GHC.Conc (forkIO)
import Data.Functor (void)

data ClockModel = ClockModel
  { clockTime   :: UTCTime
  , clockZone   :: TimeZone
  } deriving (Eq, Show)

createClockModel :: IO (TVar ClockModel)
createClockModel = do
  t <- getCurrentTime
  z <- getTimeZone t
  clockVar <- newTVarIO $ ClockModel
    { clockTime = t
    , clockZone = z
    }
  void $ forkIO $ updateClock clockVar
  pure clockVar
  where
    updateClock var = forever $ do
      t <- getCurrentTime
      z <- getTimeZone t
      atomically $ modifyTVar' var (\m -> m { clockTime = t, clockZone = z })
      threadDelay 200000
```

## Demo

https://github.com/user-attachments/assets/269a8947-8514-4d00-84e6-537050bf25fb

----

### Disclaimer

I have no idea if I'll actually maintain or develop this further. It's just a proof of concept to find out if other people actually like building UIs using this sort of approach.

