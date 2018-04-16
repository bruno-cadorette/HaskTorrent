module BEncoding.Parser where

import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer
import Text.Megaparsec.Byte
import Control.Monad
import Data.Void
import Data.Char
import Data.ByteString
import Data.Map

type ParsedString = ByteString
type Parser = Parsec Void ByteString

data BEncodingValue = 
    BString ParsedString 
    | BInt Int 
    | BList [BEncodingValue]
    | BDict (Map ParsedString BEncodingValue)
    deriving (Show, Ord, Eq)

byteChar = char . fromIntegral . ord 
bencodingString = do
    num <- decimal
    byteChar ':'
    pack <$> replicateM num anyChar 

bencodingInt = do
    byteChar 'i'
    n <- decimal
    byteChar 'e'
    return $ BInt n

bencodingList = do
    byteChar 'l'
    xs <- many bencodingValue
    byteChar 'e'
    return $ BList xs

bencodingKeyPair = do
    str <- bencodingString
    val <- bencodingValue
    return (str, val)

bencodingDict = do
    byteChar 'd'
    values <- many bencodingKeyPair
    byteChar 'e'
    return $ BDict (fromList values)
    

bencodingValue :: Parser BEncodingValue
bencodingValue = (BString <$> bencodingString) <|> bencodingInt <|> bencodingList <|> bencodingDict
