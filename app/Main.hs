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
import Control.Concurrent.STM (TVar)
import CuteBanana.Model ( Model, observeWith )
import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Core.QTimer as QTimer

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  modelVar <- createClockModel
  rootRef <- newIORef Nothing

  network <- compile $ do
    model <- observe modelVar
    vm <- createClockViewModel model
    root <- mountClock vm
    liftIO $ writeIORef rootRef (Just root)
  actuate network

  rootRef' <- readIORef rootRef
  case rootRef' of
    Nothing -> fail "The view was not built"
    Just root -> do
      QWidget.show root
      QCoreApplication.exec

observe :: Eq s => TVar s -> MomentIO (Model s)
observe = observeWith (every 16)

every :: Int -> IO () -> IO ()
every ms action = do
  timer <- QTimer.new
  connect_ timer QTimer.timeoutSignal action
  QTimer.setInterval timer ms
  QTimer.start timer ms
