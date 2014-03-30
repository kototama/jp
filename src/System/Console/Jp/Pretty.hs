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

data PState = PState { pstIndent :: Int
                     , pstLevel  :: Int
                     , pstSort   :: [(Text, Value)] -> [(Text, Value)]
                     }

data Config = Config
    { confIndent  :: Int
      -- ^ Indentation spaces per level of nesting
    , confCompare :: Text -> Text -> Ordering
      -- ^ Function used to sort keys in objects
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
defConfig = Config { confIndent = 4, confCompare = mempty }

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
    st       = PState confIndent 0 condSort
    condSort = sortBy (confCompare `on` fst)

fromValue :: PState -> Value -> Doc
fromValue st@PState{..} = go
  where
    go (Array v)  = fromArray st (V.toList v)
    go (Object m) = fromObject2 st (pstSort (H.toList m))
    go v          = fromSingleton v

fromArray :: PState -> [Value] -> Doc
fromArray st items = encloseSep lbracket rbracket comma (map (fromValue st) items)

fromObject :: PState -> [(Text, Value)] -> Doc
fromObject st items = encloseSep lbrace rbrace comma (map (\p -> fromPair p) items)
    where fromPair p = (text . unpack $ fst p) <> colon <+> (fromValue st (snd p))


fromObject2 st items = semiBraces (map (\p -> fromPair p) items)
    where fromPair p = (text . unpack $ fst p) <> colon <+> (fromValue st (snd p))

encloseSep' :: PState -> Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' = undefined

-- encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
-- encloseSep left right sep ds
--     = case ds of
--         []  -> left <> right
--         [d] -> left <> d <> right
--         _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right) 

fromSingleton v = text . TL.unpack . toLazyText $ Aeson.encodeToTextBuilder v

fromCompound :: PState
             -> (Doc, Doc)
             -> (PState -> a -> Doc)
             -> [a]
             -> Doc
fromCompound st@PState{..} (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else "\n" <> items' <> "\n" <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse ",\n" $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pstLevel = pstLevel + 1 }

fromIndent :: PState -> Doc
fromIndent PState{..} = mconcat $ replicate (pstIndent * pstLevel) " "


