{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- |Aeson-compatible colored pretty-printing of JSON 'Value's.
-- Adapted from Data.Aeson.Encode.Pretty
module System.Console.Jp.Pretty (
    -- * Simple Pretty-Printing
    encodePretty,

    -- * Pretty-Printing with Configuration Options
    encodePretty',
    Config (..), defConfig,
    -- ** Sorting Keys in Objects
    -- |With the Aeson library, the order of keys in objects is undefined due
    --  objects being implemented as HashMaps. To allow user-specified key
    --  orders in the pretty-printed JSON, 'encodePretty'' can be configured
    --  with a comparison function. These comparison functions can be composed
    --  using the 'Monoid' interface. Some other useful helper functions to keep
    --  in mind are 'comparing' and 'on'.
    --  
    --  Consider the following deliberately convoluted example, demonstrating
    --  the use of comparison functions:
    --
    --  An  object might pretty-print as follows
    --
    --  > {
    --  >   "baz": ...,
    --  >   "bar": ...,
    --  >   "foo": ...,
    --  >   "quux": ...,
    --  > }
    --
    --  which is clearly a confusing order of keys. By using a comparison
    --  function such as
    --
    --  > comp :: Text -> Text -> Ordering
    --  > comp = keyOrder ["foo","bar"] `mappend` comparing length
    --
    --  we can achieve the desired neat result:
    --
    --  > {
    --  >   "foo": ...,
    --  >   "bar": ...,
    --  >   "baz": ...,
    --  >   "quux": ...,
    --  > }
    --

    mempty,
    -- |Serves as an order-preserving (non-)sort function. Re-exported from
    --  "Data.Monoid".
    compare,
    -- |Sort keys in their natural order, i.e. by comparing character codes.
    -- Re-exported from the Prelude and "Data.Ord"
    keyOrder
) where

import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Ord
import Data.Text (Text, unpack)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text.Lazy as TL

data PState = PState { pstSort   :: [(Text, Value)] -> [(Text, Value)]
                     , pstBeforeSep :: Doc
                     , pstAfterSep :: Doc
                     }

data Config = Config
    { confIndent  :: Int
      -- ^ Indentation spaces per level of nesting
    , confCompare :: Text -> Text -> Ordering
      -- ^ Function used to sort keys in objects
    , beforeSep :: String
      -- ^ The separator displayed before the item
    , afterSep :: String
      -- ^ The separator displayed after the item
    }

-- |Sort keys by their order of appearance in the argument list.
--
--  Keys that are not present in the argument list are considered to be greater
--  than any key in the list and equal to all keys not in the list. I.e. keys
--  not in the argument list are moved to the end, while their order is
--  preserved.
keyOrder :: [Text] -> Text -> Text -> Ordering
keyOrder ks = comparing $ \k -> fromMaybe maxBound (elemIndex k ks)


-- |The default configuration: indent by four spaces per level of nesting, do
--  not sort objects by key.
--
--  > defConfig = Config { confIndent = 4, confSort = mempty }
defConfig :: Config
defConfig = Config { confIndent = 4, confCompare = mempty, beforeSep = "", afterSep = ", " }

-- |Encodes JSON as a colored document.
--
--
--  Follows the default configuration in 'defConfig'.
encodePretty :: ToJSON a => a -> Doc
encodePretty = encodePretty' defConfig

-- |A variant of 'encodePretty' that takes an additional configuration
--  parameter.
encodePretty' :: ToJSON a => Config -> a -> Doc
encodePretty' Config{..} = fromValue st . toJSON
  where
    st       = PState condSort (string beforeSep) (string afterSep)
    condSort = sortBy (confCompare `on` fst)

fromValue :: PState -> Value -> Doc
fromValue st@PState{..} = go
  where
    go (Array v)  = fromArray st (V.toList v)
    go (Object m) = fromObject2 st (pstSort (H.toList m))
    go v          = fromSingleton v

fromArray :: PState -> [Value] -> Doc
fromArray st items = encloseSep' st lbracket rbracket (map (fromValue st) items)

fromObject :: PState -> [(Text, Value)] -> Doc
fromObject st items = encloseSep lbrace rbrace comma (map (\p -> fromPair p) items)
    where fromPair p = (text . unpack $ fst p) <> colon <+> (fromValue st (snd p))


fromObject2 st items = semiBraces (map (\p -> fromPair p) items)
    where fromPair p = (text . unpack $ fst p) <> colon <+> (fromValue st (snd p))

encloseSep' :: PState -> Doc -> Doc -> [Doc] -> Doc
encloseSep' st@PState{..} left right ds
    = case ds of
        []  -> left <> right
        [d] -> left <> d <> right
        _   -> align $ cat (zipWith3 tCat (repeat pstBeforeSep) ds (repeat pstAfterSep))
    where tCat = (\b d a -> b <> d <> a)

fromSingleton v = text . TL.unpack . toLazyText $ Aeson.encodeToTextBuilder v