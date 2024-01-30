module CustomPrelude (module X) where

import Optics.Core as X
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
