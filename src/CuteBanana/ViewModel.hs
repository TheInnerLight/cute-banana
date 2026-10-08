module CuteBanana.ViewModel
  ( Field (..)
  , Command (..)
  , newField
  , readOnlyField
  , newCommand
  , noCommand
  , onCommand
  , sink
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

-- | A value the view can both show and edit.
data Field a = Field
  { fieldValue :: Behavior a
  , setField   :: a -> IO ()
  }

newField :: a -> MomentIO (Field a)
newField a0 = do
  (addHandler, fire) <- liftIO newAddHandler
  edits <- fromAddHandler addHandler
  value <- stepper a0 edits
  pure $ Field 
    { fieldValue = value
    , setField = fire 
    }

readOnlyField :: Behavior a -> Field a
readOnlyField b = 
  Field 
    { fieldValue = b
    , setField = const $ pure ()
    }

-- | An action the view can invoke.
data Command = Command
  { commandEnabled :: Behavior Bool
  , commandFired   :: Event () 
  , runCommand     :: IO ()
  }

newCommand :: Behavior Bool -> MomentIO Command
newCommand enabled = do
  (addHandler, fire) <- liftIO newAddHandler
  raw <- fromAddHandler addHandler
  pure (Command enabled (whenE enabled raw) (fire ()))

noCommand :: Command
noCommand = Command (pure True) never (pure ())

onCommand :: Command -> IO () -> MomentIO ()
onCommand cmd action = reactimate (action <$ commandFired cmd)

sink :: Behavior a -> (a -> IO ()) -> MomentIO ()
sink b set = do
  initial <- valueBLater b
  liftIOLater (set initial)
  updates <- changes b
  reactimate' (fmap set <$> updates)
