module CustomPrelude (module X) where

import Data.Optics.Operators as X
import Optics.Core as X
import Optics.Extra as X
import Optics.State as X
import Optics.State.Operators as X
import RIO as X hiding (
    ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    lens,
    over,
    preview,
    set,
    sets,
    to,
    view,
    (%~),
    (.~),
    (^.),
    (^..),
    (^?),
 )
