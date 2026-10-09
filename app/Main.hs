module Main (main) where

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Reactive.Banana.Frameworks
import System.Environment (getArgs)
import Clock.ClockModel (createClockModel)
import Data.IORef (newIORef)
import Clock.ClockViewModel (createClockViewModel)
import Clock.ClockView (mountClock)
import GHC.IORef ( writeIORef, readIORef )
import CuteBanana.Qt.Runtime (QTRuntimeConfig(..), runQTMomentIO, registerModel, liftMomentIO)

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  modelVar <- createClockModel
  rootRef <- newIORef Nothing
  let config = 
        QTRuntimeConfig
          { qtRuntimeConfigPollFrequencyMs = 16
          }
  network <- compile $ runQTMomentIO config $ do
    (_, model) <- registerModel modelVar
    vm <- liftMomentIO $ createClockViewModel model
    root <- liftMomentIO $ mountClock vm
    liftIO $ writeIORef rootRef (Just root)
  actuate network

  rootRef' <- readIORef rootRef
  case rootRef' of
    Nothing -> fail "The view was not built"
    Just root -> do
      QWidget.show root
      QCoreApplication.exec
