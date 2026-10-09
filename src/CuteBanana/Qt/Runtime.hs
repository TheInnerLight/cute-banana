{- HLINT ignore "Use newtype instead of data" -}
module CuteBanana.Qt.Runtime
  ( QTMomentIO
  , QTRuntimeConfig(..)
  , ModelSubscription
  , runQTMomentIO
  , registerModel
  , unregisterModel
  , executeQT
  , liftMomentIO
  ) where

import Data.Unique (Unique, newUnique)
import qualified StmContainers.Map as STMMap
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadTrans (lift))
import Reactive.Banana.Frameworks (MomentIO, fromAddHandler, execute, reactimate)
import Reactive.Banana ( MonadMoment, stepper )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Fix (MonadFix)
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer
import Graphics.UI.Qtah.Signal (connect_)
import Control.Monad.STM (atomically)
import DeferredFolds.UnfoldlM (foldlM')
import CuteBanana.Model (Model (..))
import Data.Foldable (forM_)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Event.Handler (newAddHandler)
import Data.IORef ( newIORef, readIORef, writeIORef )
import Control.Monad (unless)
import Reactive.Banana.Combinators (Event)

data ModelSubscription = ModelSubscription
  { poll :: IO ()
  }

data QTRuntimeConfig = QTRuntimeConfig
  { qtRuntimeConfigPollFrequencyMs :: Int
  } deriving (Eq, Ord, Show)

data RuntimeEnv = RuntimeEnv
  { runtimeEnvModels :: STMMap.Map Unique ModelSubscription
  , runtimeEnvPollFrequencyMs :: Int
  }

newtype QTMomentIO a = QTMomentIO (ReaderT RuntimeEnv MomentIO a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadMoment)

runQTMomentIO :: QTRuntimeConfig -> QTMomentIO a -> MomentIO a
runQTMomentIO conf (QTMomentIO act) = do
  timer <- liftIO  QTimer.new
  subs <- liftIO STMMap.newIO
  liftIO $ connect_ timer QTimer.timeoutSignal $ pollSubs subs
  let env = RuntimeEnv
        { runtimeEnvModels = subs
        , runtimeEnvPollFrequencyMs = qtRuntimeConfigPollFrequencyMs conf
        }
  liftIO $ QTimer.setInterval timer $ runtimeEnvPollFrequencyMs env
  liftIO $ QTimer.start timer $ runtimeEnvPollFrequencyMs env
  runReaderT act env
  where
    pollSubs subs = do
      subs' <- atomically $ foldlM' (\xs (_, x) -> pure $ x : xs) [] $ STMMap.unfoldlM subs
      forM_ subs' poll

registerModel :: Eq a => TVar a -> QTMomentIO (Unique, Model a)
registerModel var = QTMomentIO $ do
  subs <- asks runtimeEnvModels
  key <- liftIO newUnique
  s0 <- liftIO $ readTVarIO var
  lastSeen <- liftIO (newIORef s0)
  (addHandler, fire) <- liftIO newAddHandler
  let sub = ModelSubscription { poll = pollOp lastSeen fire }
  changed <- lift $ fromAddHandler addHandler
  liftIO $ atomically $ STMMap.insert sub key subs
  value <- stepper s0 changed
  let model = Model
        { modelVar = var
        , modelValue = value
        }
  pure (key, model)
    where
      pollOp lastSeen fire = do
        s <- readTVarIO var
        old <- readIORef lastSeen
        unless (s == old) $ do
          writeIORef lastSeen s
          fire s

unregisterModel :: Unique -> QTMomentIO ()
unregisterModel key = QTMomentIO $ do
  subs <- asks runtimeEnvModels
  liftIO $ atomically $ STMMap.delete key subs

liftMomentIO :: MomentIO a -> QTMomentIO a
liftMomentIO = QTMomentIO . lift

executeQT :: Event (QTMomentIO a) -> QTMomentIO (Event a)
executeQT e = do
  run <- QTMomentIO $ asks $ \env (QTMomentIO m) -> runReaderT m env
  result <- liftMomentIO (execute (run <$> e))
  liftMomentIO $ reactimate (pure () <$ result)
  pure result
