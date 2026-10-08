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
      atomically (modifyTVar' var (\m -> m { clockTime = t, clockZone = z }))
      threadDelay 200000

