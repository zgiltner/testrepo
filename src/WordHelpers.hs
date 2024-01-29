module WordHelpers (main) where

import RIO

import Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Semigroup (First (..), Sum (..))
import qualified RIO.ByteString as BS
import qualified RIO.HashMap as HashMap
import qualified RIO.List as L

main :: IO ()
main = do
    h <- withSourceFile "words.txt" $ \i ->
        runResourceT
            $ runConduit
            $ i
            .| Conduit.linesUnboundedAscii
            .| concatMapC (slidingWindow' 3)
            .| filterC ((== 3) . BS.length)
            .| histogramC

    -- This pulls them all in memory to sort...
    let h' = L.sortOn (Down . snd) $ toList h

    withSinkFile "histogram.csv" $ \o ->
        runResourceT
            $ runConduit
            $ yieldMany h'
            .| mapC (\(First b, Sum i) -> b <> "," <> fromString (show i))
            .| Conduit.unlinesAscii
            .| o

slidingWindow' :: Int -> ByteString -> [ByteString]
slidingWindow' i b = runConduitPure $ yieldMany b .| slidingWindowC i .| sinkList

histogramC :: forall k o m. (Hashable k, Monad m) => ConduitT k o m (HashMap k (First k, Sum Int))
histogramC = foldlC (\m b -> HashMap.insertWith (<>) b (First b, Sum 1) m) HashMap.empty
