import Database.SQLite.Simple
import qualified Data.HashSet as HS

getValidWords :: IO (HS.HashSet String)
getValidWords = do
    conn <- open "dictionary.db"
    words <- query_ conn "SELECT word FROM words" :: IO [Only String]
    close conn
    return $ HS.fromList (map fromOnly words)

main :: IO ()
main = do
    validWords <- getValidWords
    print validWords
