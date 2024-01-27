module CircularZipper (
    CircularZipper (..),
    current,
    right,
    rights,
    left,
    lefts,
    fromNonEmpty,
    updateCurrent,
    findRight,
) where

import RIO hiding (lefts, rights)

import Data.Coerce (coerce)
import Data.List.NonEmpty.Zipper (Zipper)
import qualified Data.List.NonEmpty.Zipper as Z

-- | Zipper that ties the end back to the beginning
newtype CircularZipper a = CircularZipper {getCircularZipper :: Zipper a}
    deriving stock (Show)
    deriving newtype (Foldable, Functor)

instance Traversable CircularZipper where
    traverse f (CircularZipper z) = CircularZipper <$> traverse f z

current :: forall a. CircularZipper a -> a
current = coerce @(Zipper a -> a) Z.current

fromNonEmpty :: forall a. NonEmpty a -> CircularZipper a
fromNonEmpty = coerce @(NonEmpty a -> Zipper a) Z.fromNonEmpty

right :: CircularZipper a -> CircularZipper a
right (CircularZipper z) = CircularZipper $ fromMaybe (Z.start z) $ Z.right z

rights :: forall a. CircularZipper a -> [a]
rights = coerce @(Zipper a -> [a]) Z.rights

lefts :: forall a. CircularZipper a -> [a]
lefts = coerce @(Zipper a -> [a]) Z.lefts

left :: CircularZipper a -> CircularZipper a
left (CircularZipper z) = CircularZipper $ fromMaybe (Z.end z) $ Z.left z

findRight :: (a -> Bool) -> CircularZipper a -> Maybe (CircularZipper a)
findRight f z = go 0 z
  where
    go n z'
        | n == length z = Nothing
        | otherwise =
            let z'' = right z'
             in if f $ current z'' then Just z'' else go (n + 1) z''

updateCurrent :: (a -> a) -> CircularZipper a -> CircularZipper a
updateCurrent f (CircularZipper z) = CircularZipper $ Z.replace (f $ Z.current z) z
