module CaseInsensitive (CaseInsensitiveText (..), CaseInsensitiveChar (..), caseInsensitiveLetters, isInfixOf) where

import CustomPrelude

import qualified Data.Char as C
import Data.Coerce (coerce)
import Data.Hashable (Hashable (..))
import qualified Data.Text as T
import Lucid (ToHtml (..))
import qualified RIO.HashSet as HashSet
import Web.HttpApiData (FromHttpApiData)
import Web.Internal.HttpApiData (FromHttpApiData (..))

newtype CaseInsensitiveText = CaseInsensitiveText Text
    deriving stock (Show, Generic)
    deriving newtype (IsString, ToHtml)

instance Eq CaseInsensitiveText where
    (==) = coerce ((==) `on` T.toCaseFold)

instance Hashable CaseInsensitiveText where
    hashWithSalt i = coerce (hashWithSalt i . T.toCaseFold)

instance FromHttpApiData CaseInsensitiveText where
    parseQueryParam = coerce (parseQueryParam @Text)

isInfixOf :: CaseInsensitiveText -> CaseInsensitiveText -> Bool
isInfixOf = coerce (T.isInfixOf `on` T.toCaseFold)

newtype CaseInsensitiveChar = CaseInsensitiveChar Char
    deriving stock (Show, Generic)
    deriving newtype (Enum)

instance Eq CaseInsensitiveChar where
    (==) = coerce ((==) `on` C.toUpper)

instance Hashable CaseInsensitiveChar where
    hashWithSalt i = coerce (hashWithSalt i . C.toUpper)

instance ToHtml CaseInsensitiveChar where
    toHtml (CaseInsensitiveChar c) = toHtml [C.toUpper c]
    toHtmlRaw (CaseInsensitiveChar c) = toHtmlRaw [C.toUpper c]

caseInsensitiveLetters :: CaseInsensitiveText -> HashSet CaseInsensitiveChar
caseInsensitiveLetters = coerce (T.foldr (HashSet.insert . CaseInsensitiveChar) mempty)
