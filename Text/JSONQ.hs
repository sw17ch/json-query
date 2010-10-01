{-# LANGUAGE OverloadedStrings #-}
module Text.JSONQ (
    JSONV,  -- | A JSON value
    JSONQ,  -- | A query
    JSONS,  -- | A selector
    jsonv,  -- | Parse a string to a JSON value
    jsonv_, -- | Parse a string a JSON value, errors go to IO
) where

import Data.String
import qualified Data.ByteString.Char8 as B

import Text.JSON.AttoJSON

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String

-- | JSON value wrapper. Wrapper for JSValue from Text.JSON.
newtype JSONV = JSONV JSValue

instance Show JSONV where
    show (JSONV jsv) = "JSONS << " ++ (B.unpack $ showJSON jsv) ++ " >>"

-- | Convert a string to either a JSONS or a parse error.
jsonv :: String -> Either String JSONV
jsonv s = case parseJSON (B.pack s) of
                Right j -> Right $ JSONV j
                Left e  -> Left e

-- | Convert a string to a JSONS. If there is a parse
-- error, an IO error will be raised.
jsonv_ :: String -> JSONV
jsonv_ s = case jsonv s of
                Right j -> j
                Left e -> error e

newtype JSONQ = JSONQ [JSONS]
    deriving (Show)

data JSONS = Key String
           | Idx Int
    deriving (Show)

parseJSONQ :: Parser JSONQ
parseJSONQ = do
    q <- parseSet `sepBy1` (char '.')
    eof
    
    return . JSONQ $ concat q

parseSet :: Parser [JSONS]
parseSet = do
    k <- parseKey
    i <- many $ parseIdx

    return (k : i)

parseJSONS :: Parser JSONS 
parseJSONS = try parseIdx <|> try parseKey

parseKey :: Parser JSONS
parseKey = try parseQuotedKey
       <|> try parseNormKey

parseNormKey :: Parser JSONS
parseNormKey = many1 alphaNum >>= return . Key

parseQuotedKey :: Parser JSONS
parseQuotedKey = do
    char '\''
    k <- many (noneOf "\\'")
    char '\''
    return $ Key k

parseIdx :: Parser JSONS
parseIdx = do
    char '['
    d <- many1 digit
    char ']'

    return . Idx . read $ d

{- Test data -}

input :: String
input = "{ \"the name\": \"john\", \"age\": 24, \"pets\": [ { \"name\": \"Malcolm\", \"type\": \"cat\" }, { \"name\": \"River\", \"type\": \"cat\" }, { \"name\": \"Coach\", \"type\": \"dog\" } ] } "

input2 = "{\"\": \"blank\"}"

query1, query2 :: String
query1 = "pets[1].name"
query2 = "'the name'"

