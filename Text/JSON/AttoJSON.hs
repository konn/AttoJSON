{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Text.JSON.AttoJSON (
    -- * Class and Data-Types for JSON Value
    JSValue (..), JSON(..),
    -- * Parsing & Printing
    parseJSON, readJSON, showJSON, showJSON', 
    -- * Manipulating Objects
    lookup, getField, findWithDefault,
    lookupDeep, getFields, findDeepWithDefault,
    updateField
  ) where

import Control.Applicative hiding (many)
import qualified Data.ByteString.Lazy.Char8 as L (toChunks)
import Data.ByteString.Char8 
                       ( append, ByteString, unfoldr, take, reverse
                       , singleton, intercalate, concat
                       , foldl'
                       )
import Data.Attoparsec ( Parser, maybeResult, eitherResult)
import Data.Traversable (mapM)
import Data.Attoparsec.Char8 ( char8, parse, string, decimal, skipSpace, satisfy
                             , inClass, sepBy, option, many, endOfInput, try, feed
                             , takeWhile1, isDigit
                             )
import Prelude hiding (concatMap, reverse, replicate, take
                      , Show(..), concat, mapM, lookup)
import qualified Prelude as P (Show, reverse, map)
import Data.Foldable (foldrM)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Control.Arrow (second, (***))
import Data.ByteString.UTF8 (fromString, toString)
import Text.Show.ByteString (show)
import Data.Ratio (denominator, numerator, (%))
import Numeric (readHex)
import Data.Map (Map, fromList, elems, mapWithKey, toList, mapKeys, insert)
import qualified Data.Map as M (lookup)
import Data.Generics (Data(..), Typeable(..))
import Control.Monad (replicateM, guard)
import Data.Maybe (fromMaybe)

fromLazy = concat . L.toChunks

-- |Data types for JSON value.
data JSValue = JSString {fromJSString :: !ByteString} -- ^ JSON String
             | JSNumber Rational                      -- ^ JSON Number
             | JSObject (Map ByteString JSValue)      -- ^ JSON Object
             | JSArray [JSValue]                      -- ^ JSON Array
             | JSBool !Bool                           -- ^ JSON Bool
             | JSNull                                 -- ^ JSON Null
               deriving (P.Show, Eq, Data, Typeable)

-- |Get the value for field in Object and decode it.
lookup :: JSON a => ByteString -> JSValue -> Maybe a
lookup key (JSObject dic) = M.lookup key dic >>= fromJSON
lookup _ _                = Nothing

-- |'lookup' with default value.
findWithDefault :: JSON a => a -> ByteString -> JSValue -> a
findWithDefault def key = fromMaybe def . getField key

{-# DEPRECATED getField "Use lookup" #-}
-- | DEPRECATED: Alias of 'lookup'. Use 'lookup'.
getField :: JSON a => ByteString -> JSValue -> Maybe a
getField = lookup

-- |Same as 'lookup' but it can process nested Object. ex:
-- 
-- @
--   lookupDeep [\"user\", \"name\"] (JSObject [(\"user\", JSObject [(\"name\", JSString \"hoge\")])]) == Just \"hoge\"
-- @
lookupDeep :: JSON a => [ByteString] -> JSValue -> Maybe a
lookupDeep keys jso = fromJSON =<< foldrM lookup jso (P.reverse keys)

-- |'getFields' with default value.
findDeepWithDefault :: JSON a => a -> [ByteString] -> JSValue -> a
findDeepWithDefault def keys = fromMaybe def . lookupDeep keys

{-# DEPRECATED getFields "Use lookupDeep" #-}
-- | DEPRECATED: Alias of 'lookupDeep'
getFields :: JSON a => [ByteString] -> JSValue -> Maybe a
getFields  = lookupDeep

-- | Update or Insert the value for field in Object.
updateField :: JSON a => ByteString -> a -> JSValue -> JSValue
updateField key v (JSObject jso) = JSObject $ insert key (toJSON v) jso
updateField _ _ j                = j

-- |Type Class for the value that can be converted from/into 'JSValue'.
class JSON a where
    -- |Decode from JSValue
    fromJSON :: JSValue -> Maybe a
    -- |Encode into JSValue
    toJSON   :: a -> JSValue

instance JSON JSValue where
    fromJSON = return
    toJSON   = id

instance JSON Rational where
    fromJSON (JSNumber r) = Just r
    fromJSON _            = Nothing
    toJSON                = JSNumber

instance JSON Int where
    fromJSON (JSNumber a) = fromInteger <$> fromJSON (JSNumber a)
    fromJSON (JSString a) = Nothing
    fromJSON _            = Nothing
    toJSON a              = JSNumber (fromIntegral a)

instance JSON Integer where
    fromJSON (JSNumber a) | denominator a == 1 = Just $ numerator a
                          | otherwise          = Nothing
    fromJSON _            = Nothing
    toJSON a              = JSNumber (fromIntegral a)

instance JSON Double where
    fromJSON (JSNumber a) = Just $ fromRational a
    fromJSON _            = Nothing
    toJSON a              = JSNumber $ toRational a

instance JSON ByteString where
    fromJSON (JSString a) = Just a
    fromJSON _            = Nothing
    toJSON                = JSString

instance JSON String where
    fromJSON (JSString a) = Just $ toString a
    fromJSON _            = Nothing
    toJSON str            = JSString $ fromString str

instance JSON Bool where
    fromJSON (JSBool bl) = Just bl
    fromJSON _           = Nothing
    toJSON               = JSBool

instance JSON () where
    fromJSON JSNull       = Just ()
    fromJSON _            = Nothing
    toJSON ()             = JSNull

instance JSON a => JSON [a] where
    fromJSON (JSArray xs) = mapM fromJSON xs
    fromJSON _            = Nothing
    toJSON                = JSArray . map toJSON

instance JSON a => JSON [(String, a)] where
    fromJSON (JSObject d) = mapM (\(k, v)-> (,) (toString k) <$> fromJSON v) $ toList d
    fromJSON  _           = Nothing
    toJSON dic            = JSObject $ fromList $ P.map (fromString *** toJSON) dic

instance JSON a => JSON [(ByteString, a)] where
    fromJSON (JSObject d) = mapM (\(k, v) -> (,) k <$> fromJSON v) $ toList d
    fromJSON _            = Nothing
    toJSON dic            = JSObject $ fromList $ P.map (second toJSON) dic

instance JSON a => JSON (Map String a) where
    fromJSON (JSObject d) = mapM fromJSON $ mapKeys toString d
    fromJSON _            = Nothing
    toJSON                = JSObject . fmap toJSON . mapKeys fromString

instance JSON a => JSON (Map ByteString a) where
    fromJSON (JSObject d) = mapM fromJSON d
    fromJSON _            = Nothing
    toJSON                = JSObject . fmap toJSON

instance JSON a => JSON (Maybe a) where
    fromJSON JSNull = Just Nothing
    fromJSON jso    = fromJSON jso
    toJSON (Just a) = toJSON a
    toJSON Nothing  = JSNull

lexeme p = skipSpace *> p
symbol s = lexeme $ string s 

value :: Parser JSValue
value = jsString <|> number <|> object <|> array <|> try bool <|> try jsNull

-- |Parse JSON source. Returns 'JSValue' ('Right') if succeed, Returns 'Left' if failed.
--
--  The input string should be UTF8-encoded.  Unicode escapes (e.g. @\"\\u266B\"@) are encoded in UTF8
--  by the parser, so incompatibilities will arise if you try to use AttoJSON with other encodings.
parseJSON :: ByteString -> Either String JSValue
parseJSON = eitherResult . flip feed "" . parse (value <* skipSpace <* endOfInput)

-- |Maybe version of 'parseJSON'.
readJSON :: ByteString -> Maybe JSValue
readJSON = maybeResult . flip feed "" . parse (value <* skipSpace <* endOfInput)

-- |Print 'JSValue' as JSON source (not pretty).
--
--  The output string will be in UTF8 (provided the JSValue was constructed with UTF8 strings).
showJSON :: JSValue -> ByteString
showJSON (JSObject dic) = "{" `append` intercalate "," mems `append` "}"
  where
    mems = elems $ mapWithKey (\k v -> showJSString False k `append` ":" `append` showJSON v) dic
showJSON (JSString jss)     = showJSString False jss
showJSON (JSNumber jsn) | denominator jsn == 1 = fromLazy $ show $ numerator jsn
                        | otherwise            = fromLazy $ show (fromRational jsn :: Double)
showJSON (JSArray jss)      = "[" `append` intercalate ", " (P.map showJSON jss) `append` "]"
showJSON (JSNull)           = "null"
showJSON (JSBool True)      = "true"
showJSON (JSBool False)     = "false"

-- |Same as 'showJSON', but escape Unicode Charactors.
showJSON' :: JSValue -> ByteString
showJSON' (JSObject dic) = "{" `append` intercalate "," mems `append` "}"
  where
    mems = elems $ mapWithKey (\k v -> showJSString True k `append` ":" `append` showJSON v) dic
showJSON' (JSString jss) = showJSString True jss
showJSON' jsv            = showJSON jsv

showJSString :: Bool -> ByteString -> ByteString
showJSString escapeU js = "\"" `append` escape js `append` "\""
  where
    escape :: ByteString -> ByteString
    escape = concat . P.map escapeCh . toString

    escapeCh :: Char -> ByteString
    escapeCh '\\' = "\\\\"
    escapeCh '"'  = "\\\""
    escapeCh '\b' = "\\b"
    escapeCh '\f' = "\\f"
    escapeCh '\n' = "\\n"
    escapeCh '\r' = "\\r"
    escapeCh '\t' = "\\t"
    escapeCh ch | mustEscape ch || (escapeU && ch > '\xff') = escapeHex $ fromEnum ch
                | otherwise           = fromString [ch]

jsNull = lexeme (JSNull <$ symbol "null")
bool   = lexeme $
           JSBool True  <$ symbol "true"
       <|> JSBool False <$ symbol "false"
number = lexeme $ do
  sig   <- option 1 (-1 <$ char8 '-')
  int   <- decimal
  float <- option 0 frac
  pow  <- option 1 power
  return $ JSNumber (sig*(fromIntegral int+float)*pow)

frac :: Parser Rational
frac = do
  let step (a,p) w = (a * 10 + fromIntegral (fromEnum w - 48), p * 10)
      finish (a,p) = a % p
  char8 '.'
  finish . foldl' step (0,1) <$> takeWhile1 isDigit

power = do
  string "E" <|> string "e"
  sgn <- option 1 ((1 <$ char8 '+') <|> (-1 <$ char8 '-'))
  pow <- decimal
  return (10^^(sgn*pow))

array  = lexeme (symbol "[" *> (JSArray <$> (value `sepBy` symbol ",")) <* symbol "]")

isControlChar ch = ('\NUL' <= ch && ch <= '\US') || ch == '\DEL'
isNotHadaka ch = ch `elem` "\"\\"


jsString :: Parser JSValue
jsString = lexeme ( string "\"" *> (JSString . concat <$> many jsChar) <* string "\"")
jsChar :: Parser ByteString
jsChar = singleton <$> satisfy (\a -> not (isControlChar a || isNotHadaka a))
     <|> string "\\" *> (escapeChar <|> escapeUnicode)

escapeChar :: Parser ByteString
escapeChar = string "\"" 
         <|> string "\\" 
         <|> "\b" <$ char8 'b'
         <|> "\f" <$ char8 'f'
         <|> "/"  <$ char8 '/'
         <|> "\n" <$ char8 'n'
         <|> "\r" <$ char8 'r'
         <|> "\t" <$ char8 't'

-- Parse either a single hex digit, or (if present) a surrogate pair.
escapeUnicode :: Parser ByteString
escapeUnicode = (\x -> fromString [toEnum x]) <$> do
  uc <- hex4digit
  if uc >= 0xd800 && uc <= 0xdbff
    then try $ do
           lc <- string "\\" *> hex4digit
           guard (lc >= 0xdc00 && uc < 0xdfff)
           return $ fromSurrogatePair (uc,lc)
     <|> return uc
    else return uc

hex4digit :: Parser Int
hex4digit = do
  string "u"
  ((hex, _):_) <- readHex <$> replicateM 4 (satisfy $ inClass "0-9a-zA-Z")
  return hex

object = lexeme ( symbol "{" *> (JSObject . fromList <$> (objMember `sepBy` symbol ",")) <* symbol "}")

objMember :: Parser (ByteString, JSValue)
objMember = lexeme ((,) <$> (fromJSString <$> jsString) <*> (symbol ":" *> value))

unfoldrStep :: (a -> Bool) -> (a -> Char) -> (a -> a) -> a -> Maybe (Char, a)
unfoldrStep p f g a | p a       = Nothing
                    | otherwise = Just (f a, g a)

mustEscape ch = ch < ' ' || ch == '\x7f'

escapeHex :: Int -> ByteString
escapeHex c | c < 0x10000 = show4Hex c
            | otherwise   = astral c

show4Hex :: Int -> ByteString
show4Hex = ("\\u" `append`) . reverse . take 4 . (`append` "0000") . unfoldr (unfoldrStep (==0) (toHexChar .(`mod` 16)) (`div` 16)) 

astral :: Int -> ByteString
astral n = show4Hex a `append` show4Hex b
  where (a,b) = toSurrogatePair n

fromSurrogatePair :: (Int,Int) -> Int
fromSurrogatePair (uc,lc) = 0x10000 .|. a .|. b where
  a = (uc .&. 0x3ff) `shiftL` 10
  b = lc .&. 0x3ff

toSurrogatePair :: Int -> (Int,Int)
toSurrogatePair n = (a + 0xd800, b + 0xdc00) where
  a = (n `shiftR` 10) .&. 0x3ff
  b = n .&. 0x3ff

toHexChar :: Int -> Char
toHexChar d | 0 <= d && d <= 9 = toEnum $ 48 + d
            | 10 <= d          = toEnum $ 87 + d 
