module CuteBanana.Model
  ( Model (..)
  ) where

import Control.Concurrent.STM
import Reactive.Banana
data Model s = Model
  { modelVar   :: TVar s
  , modelValue :: Behavior s
  }
