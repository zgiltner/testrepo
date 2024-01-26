module App (AppM, App (..)) where

import Control.Monad.Reader (ReaderT)
import Servant (Handler)

type AppM = ReaderT App Handler

data App = App {}
