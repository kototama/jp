{-# LANGUAGE OverloadedStrings, RecordWildCards #-}


-- |Aeson-compatible colored pretty-printing of JSON 'Value's.
-- Adapted from Data.Aeson.Encode.Pretty
module System.Console.Jp.Pretty (
    -- * Simple Pretty-Printing
    encodePretty,
    
    -- * Pretty-Printing with Configuration Options
    encodePretty',
    Config (..), defConfig, compactConfig, nocolorConfig,
    
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
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Function (on)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Ord
import Data.Text (Text, unpack)
import Data.Text.Lazy.Builder (toLazyText)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text.Lazy as TL

data PState = PState { pstSort   :: [(Text, Value)] -> [(Text, Value)]
                     , pstIndent :: Int
                     , pstBeforeSep :: Doc
                     , pstAfterSep :: Doc
                     , pstPairSep :: Doc
                     , pstCatArray :: [Doc] -> Doc
                     , pstArrayPrefix :: Doc
                     , pstArraySuffix :: Doc
                     , pstCatObject :: [Doc] -> Doc
                     , pstObjectPrefix :: Doc
                     , pstObjectSuffix :: Doc
                     , pstStringFn :: Doc -> Doc
                     , pstNumberFn :: Doc -> Doc
                     , pstBoolFn :: Doc -> Doc
                     , pstNullFn :: Doc -> Doc
                     }

data Config = Config
    { confIndent  :: Int
      -- ^ Indentation spaces per level of nesting
    , confCompare :: Text -> Text -> Ordering
      -- ^ Function used to sort keys in objects
    , beforeSep :: Doc
      -- ^ The separator displayed before the item
    , afterSep :: Doc
      -- ^ The separator displayed after the item
    , pairSep :: Doc
      -- ^ The separator displayed between the key and the value of an object
    , catObject :: [Doc] -> Doc
    -- ^ The function to concatenate document representing object's items
    , catArray  :: [Doc] -> Doc
    -- ^ The function to concatenate document representing array's items
    , arrayPrefix :: Doc
    -- ^ The prefix of the array's elements
    , arraySuffix :: Doc
    -- ^ The suffix of the array's elements
    , objectPrefix :: Doc
    -- ^ The prefix of the object's elements
    , objectSuffix :: Doc
    -- ^ The suffix of the object's elements
    , stringFn :: Doc -> Doc
    -- ^ The function to apply to document representing a string
    , numberFn :: Doc -> Doc
    -- ^ The function to apply to document representing a number
    , boolFn :: Doc -> Doc
    -- ^ The function to apply to document representing a boolean
    , nullFn :: Doc -> Doc
    -- ^ The function to apply to document representing a null value
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
defConfig = Config { confIndent = 4
                   , confCompare = mempty
                   , beforeSep = empty
                   , afterSep = (comma <> empty)
                   , pairSep = (colon <> space)
                   , catObject = cat
                   , catArray = cat
                   , arrayPrefix = (lbracket <$> empty)
                   , arraySuffix = (empty <$> rbracket)
                   , objectPrefix = (lbrace <$> empty)
                   , objectSuffix = (empty <$> rbrace)
                   , stringFn = dullgreen
                   , numberFn = dullmagenta
                   , boolFn = dullred
                   , nullFn = white
                   }

nocolorConfig :: Config
nocolorConfig = defConfig { stringFn = id
                          , numberFn = id
                          , boolFn = id
                          , nullFn = id
                          }

compactConfig :: Config
compactConfig = defConfig { confIndent = 0,
                            arrayPrefix = lbracket,
                            arraySuffix = rbracket,
                            objectPrefix = lbrace,
                            objectSuffix = rbrace,
                            catObject = hcat,
                            catArray = hcat
                          }
                
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
    st       = PState { pstSort = condSort
                      , pstIndent = confIndent
                      , pstBeforeSep = beforeSep
                      , pstAfterSep = afterSep
                      , pstPairSep = pairSep
                      , pstCatArray = catArray
                      , pstArrayPrefix = arrayPrefix
                      , pstArraySuffix = arraySuffix
                      , pstCatObject = catObject
                      , pstObjectPrefix = objectPrefix
                      , pstObjectSuffix = objectSuffix
                      , pstStringFn = stringFn
                      , pstNumberFn = numberFn
                      , pstBoolFn = boolFn
                      , pstNullFn = nullFn
                      }
    condSort = sortBy (confCompare `on` fst)

fromValue :: PState -> Value -> Doc
fromValue st@PState{..} = go
  where
    go (Array v)  = fromArray st (V.toList v)
    go (Object m) = fromObject st (pstSort (H.toList m))
    go v          = fromScalar st v

fromArray :: PState -> [Value] -> Doc
fromArray st@PState{..} items = pstArrayPrefix <> arrayContent <> pstArraySuffix
    where ds = (map (fromValue st) items)
          arrayContent = indent pstIndent $ (pstCatArray (punctuate' st ds))

fromObject :: PState -> [(Text,Value)] -> Doc
fromObject st@PState{..} items = pstObjectPrefix <> objectContent <> pstObjectSuffix
    where ds = (map (fromPair st) items)
          objectContent = indent pstIndent $ (pstCatObject (punctuate' st ds))

fromPair :: PState -> (Text,Value) -> Doc
fromPair st@PState{..} p = (text . unpack $ fst p) <> pstPairSep <> (fromValue st (snd p))

punctuate' :: PState -> [Doc] -> [Doc]
punctuate' _ []      = []
punctuate' _ [d]     = [d]
punctuate' st@PState{..} (d:ds)  = (pstBeforeSep <> d <> pstAfterSep) : punctuate' st ds

fromScalar :: PState -> Value -> Doc
fromScalar PState{..} v@(Aeson.String _) = pstStringFn $ scalarToText v
fromScalar PState{..} v@(Aeson.Number _) = pstNumberFn $ scalarToText v 
fromScalar PState{..} v@(Aeson.Bool _) = pstBoolFn $ scalarToText v 
fromScalar PState{..} v@Aeson.Null = pstNullFn $ scalarToText v 
fromScalar _ v = scalarToText v

scalarToText :: Value -> Doc
scalarToText v = text . TL.unpack . toLazyText $ encodeToTextBuilder v


