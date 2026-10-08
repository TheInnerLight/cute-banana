module CuteBanana.Model
  ( Model (..)
  , observeWith
  , update
  , modelField
  ) where

import Control.Concurrent.STM
import Control.Monad (unless)
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks
import CuteBanana.ViewModel (Field (..))

data Model s = Model
  { modelVar   :: TVar s
  , modelValue :: Behavior s
  }

observeWith :: Eq s => (IO () -> IO ()) -> TVar s -> MomentIO (Model s)
observeWith schedule var = do
  s0 <- liftIO (readTVarIO var)
  lastSeen <- liftIO (newIORef s0)
  (addHandler, fire) <- liftIO newAddHandler
  changed <- fromAddHandler addHandler
  value <- stepper s0 changed
  liftIO $ schedule $ do
    s <- readTVarIO var
    old <- readIORef lastSeen
    unless (s == old) $ do
      writeIORef lastSeen s
      fire s
  pure $ Model
    { modelVar = var
    , modelValue = value
    }

update :: Model s -> (s -> s) -> IO ()
update model f = atomically (modifyTVar' (modelVar model) f)

modelField :: Model s -> (s -> a) -> (a -> s -> s) -> Field a
modelField model get set = Field
  { fieldValue = get <$> modelValue model
  , setField   = update model . set
  }
