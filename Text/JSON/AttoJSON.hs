{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Text.JSON.AttoJSON (JSValue (..), getField, getFields, JSON(..), parseJSON, readJSON, showJSON) where
import Control.Applicative hiding (many)
import qualified Data.ByteString.Lazy as L (unpack)
import Control.Monad.Identity (runIdentity)
import Data.ByteString ( append, ByteString, replicate, unfoldr, take, reverse
                       , singleton, pack, intercalate, concat, unpack
                       )
import Data.Attoparsec ( maybeResult, parse, string, try, Parser, endOfInput
                       , parseWith, eitherResult, inClass, many, count, sepBy
                       , satisfy, option 
                       )
import Data.Attoparsec.Char8 (char8, decimal, skipSpace, signed)
import Prelude hiding (concatMap, reverse, replicate, take, Show(..), concat)
import qualified Prelude as P (Show, reverse, map)
import Data.Foldable (foldrM)
import Data.Bits (shiftR, (.&.))
import Codec.Binary.UTF8.String (encode, decode)
import Data.Word (Word8)
import Text.Show.ByteString (show)
import Data.Ratio (denominator, numerator)
import Numeric (readHex)
import Data.Map (Map, fromList, elems, mapWithKey)
import qualified Data.Map as M (lookup)

fromLazy = pack . L.unpack

data JSValue = JSString {fromJSString :: !ByteString}
             | JSNumber Rational
             | JSObject (Map ByteString JSValue)
             | JSArray [JSValue]
             | JSBool !Bool
             | JSNull deriving (P.Show, Eq)

getField :: JSON a => ByteString -> JSValue -> Maybe a
getField key (JSObject dic) = M.lookup key dic >>= fromJSON
getField _ _                = Nothing

getFields :: JSON a => [ByteString] -> JSValue -> Maybe a
getFields keys jso = fromJSON =<< foldrM getField jso (P.reverse keys)

class JSON a where
    fromJSON :: JSValue -> Maybe a
    toJSON   :: a -> JSValue

instance JSON JSValue where
    fromJSON = return
    toJSON   = id

instance JSON Int where
    fromJSON (JSNumber a) = fromInteger <$> fromJSON (JSNumber a)
    fromJSON (JSString a) = maybeResult $ parse (signed decimal) a
    fromJSON _            = Nothing
    toJSON a              = JSNumber (fromIntegral a)

instance JSON Integer where
    fromJSON (JSNumber a) | denominator a == 1 = Just $ numerator a
                          | otherwise          = Nothing
    fromJSON (JSString a) = maybeResult $ parse (signed decimal) a
    fromJSON _            = Nothing
    toJSON a              = JSNumber (fromIntegral a)

instance JSON Double where
    fromJSON (JSNumber a) = Just $ fromRational a
    fromJSON _            = Nothing
    toJSON a              = JSNumber $ toRational a

instance JSON ByteString where
    fromJSON (JSString a) = Just a
    fromJSON _            = Nothing
    toJSON str            = JSString str

instance JSON String where
    fromJSON (JSString a) = Just $ w8sToStr $ unpack a
    fromJSON _            = Nothing
    toJSON str            = JSString $ pack $ strToW8s str

lexeme p = skipSpace *> p
symbol s = lexeme $ string s 

value :: Parser JSValue
value = jsString <|> number <|> object <|> array <|> try bool <|> try jsNull

parseJSON :: ByteString -> Either String JSValue
parseJSON = eitherResult . runIdentity . parseWith (return "") (value <* skipSpace <* endOfInput)
readJSON :: ByteString -> Maybe JSValue
readJSON = maybeResult . runIdentity . parseWith (return "") (value <* skipSpace <* endOfInput)

jsNull = lexeme (JSNull <$ symbol "null")
bool   = lexeme $
           JSBool True  <$ symbol "true"
       <|> JSBool False <$ symbol "false"
number = lexeme $ do
  sig   <- option 1 $ (-1 <$ char8 '-')
  int   <- decimal
  float <- option 0 frac
  pow  <- option 1 power
  return $ JSNumber (sig*(fromIntegral int+float)*pow)

frac = do
  char8 '.'
  dgs <- fromIntegral <$> decimal
  return (dgs / (10^^ceiling(logBase 10 (fromRational dgs))) )

power = do
  string "E" <|> string "e"
  sgn <- option 1 ((1 <$ char8 '+') <|> (-1 <$ char8 '-'))
  pow <- decimal
  return (10^^(sgn*pow))

array  = lexeme (symbol "[" *> (JSArray <$> (value `sepBy` (symbol ","))) <* symbol "]")

isControlChar ch = (0 <= ch && ch <= 31) || ch == 127
isNotHadaka ch = ch == 34 || ch == 92

w8sToStr :: [Word8] -> String
w8sToStr = map (toEnum . fromEnum)

strToW8s :: String -> [Word8]
strToW8s = map (toEnum . fromEnum)

chToW8 :: Char -> Word8
chToW8 = toEnum . fromEnum

w8ToCh :: Char -> Word8
w8ToCh = toEnum . fromEnum

jsString :: Parser JSValue
jsString = lexeme ( string "\"" *> (JSString . concat <$> many jsChar) <* string "\"")
jsChar :: Parser ByteString
jsChar = singleton <$> satisfy (\a -> not (isControlChar a || isNotHadaka a))
     <|> string "\\" *> (escapeChar <|> hex4digit)

escapeChar :: Parser ByteString
escapeChar = string "\"" 
         <|> string "\\" 
         <|> "\b" <$ char8 'b'
         <|> "\f" <$ char8 'f'
         <|> "/"  <$ char8 '/'
         <|> "\n" <$ char8 'n'
         <|> "\r" <$ char8 'r'
         <|> "\t" <$ char8 't'

hex4digit :: Parser ByteString
hex4digit = do
  string "u"
  ((hex, _):_) <- readHex . w8sToStr <$> count 4 (satisfy $ inClass "0-9a-zA-Z")
  return $ pack $ encode $ [toEnum hex]
object = lexeme ( symbol "{" *> (JSObject . fromList <$> (objMember `sepBy` symbol ",")) <* symbol "}")

objMember :: Parser (ByteString, JSValue)
objMember = lexeme ((,) <$> (fromJSString <$> jsString) <*> (symbol ":" *> value))

showJSON :: JSValue -> ByteString
showJSON (JSObject dic) = "{" `append` intercalate "," mems `append` "}"
  where
    mems = elems $ mapWithKey (\k v -> showJSString k `append` ":" `append` showJSON v) dic
showJSON (JSString jss)     = showJSString jss
showJSON (JSNumber jsn) | denominator jsn == 1 = fromLazy $ show $ numerator jsn
                        | otherwise            = fromLazy $ show (fromRational jsn :: Double)
showJSON (JSArray jss)      = "[" `append` intercalate ", " (P.map showJSON jss) `append` "]"
showJSON (JSNull)           = "null"
showJSON (JSBool True)      = "true"
showJSON (JSBool False)     = "false"

showJSString :: ByteString -> ByteString
showJSString js = "\"" `append` escape js `append` "\""
  where
    escape :: ByteString -> ByteString
    escapeCh :: Char -> ByteString
    escape = concat . P.map escapeCh . decode . unpack
    escapeCh '\\' = "\\\\"
    escapeCh '"'  = "\\\""
    escapeCh '\b' = "\\b"
    escapeCh '\f' = "\\f"
    escapeCh '/'  = "\\/"
    escapeCh '\n' = "\\n"
    escapeCh '\r' = "\\r"
    escapeCh '\t' = "\\t"
    escapeCh ch | mustEscape ch    = escapeHex $ fromEnum ch
                | otherwise        = singleton $ chToW8 ch

unfoldrStep :: (a -> Bool) -> (a -> Word8) -> (a -> a) -> a -> Maybe (Word8, a)
unfoldrStep p f g a | p a       = Nothing
                    | otherwise = Just (f a, g a)

mustEscape ch = ch < ' ' || ch == '\x7f' || ch > '\xff'

escapeHex :: Int -> ByteString
escapeHex c | c < 0x10000 = show4Hex c
            | otherwise   = astral c

show4Hex :: Int -> ByteString
show4Hex = ("\\u" `append`) . reverse . take 4 . (`append` replicate 4 48) . unfoldr (unfoldrStep (==0) (toHexWord8 .(`mod` 16)) (`div` 16)) 

astral :: Int -> ByteString
astral n = show4Hex (a + 0xd800) `append` show4Hex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

toHexWord8 :: Int -> Word8
toHexWord8 d | 0 <= d && d <= 9 = 48 + toEnum d
             | 10 <= d          = 87 + toEnum d 