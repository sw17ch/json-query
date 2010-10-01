{-# LANGUAGE OverloadedStrings #-}
module Text.JSONQ (
    JSONV,  -- | A JSON value
    JSONQ,  -- | A query
    JSONS,  -- | A selector
    jsonv,  -- | Parse a string to a JSON value
    jsonv_, -- | Parse a string a JSON value, errors go to IO

    showJSON, -- From Text.JSON.AttoJSON
    check,

    q,
    q_,
) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Text.JSON.AttoJSON

import Text.Parsec
import Text.Parsec.ByteString

-- | JSON value wrapper. Wrapper for JSValue from Text.JSON.
type JSONV = JSValue

-- | Convert a string to either a JSONS or a parse error.
jsonv :: B.ByteString -> Either String JSONV
jsonv s = parseJSON s

-- | Convert a string to a JSONS. If there is a parse
-- error, an IO error will be raised.
jsonv_ :: B.ByteString -> JSONV
jsonv_ s = case jsonv s of
                Right j -> j
                Left e -> error e

type JSONQ = [JSONS]

data JSONS = Key B.ByteString
           | Idx Int
    deriving (Show)

parseJSONQ :: Parser JSONQ
parseJSONQ = do
    qry <- parseSet `sepBy1` (char '.')
    eof
    
    return $ concat qry

parseSet :: Parser [JSONS]
parseSet = do
    k <- parseKey
    i <- many $ parseIdx

    return (k : i)

parseKey :: Parser JSONS
parseKey = try parseQuotedKey
       <|> try parseNormKey

parseNormKey :: Parser JSONS
parseNormKey = many1 alphaNum >>= return . Key . B.pack

parseQuotedKey :: Parser JSONS
parseQuotedKey = do
    _ <- char '\''
    k <- many (noneOf "\\'")
    _ <- char '\''
    return . Key . B.pack $ k

parseIdx :: Parser JSONS
parseIdx = do
    _ <- char '['
    d <- many1 digit
    _ <- char ']'

    return . Idx . read $ d

parseQ :: B.ByteString -> Either ParseError JSONQ
parseQ = parse parseJSONQ "json-query"

-- | Tries to parse a query. True if valid, False if broken.
check :: B.ByteString -> Bool
check s = case parseQ s of
            Left _ -> False
            Right _ -> True

q :: JSONV -> B.ByteString -> Either String JSONV
q v l' = run v l
    where Right l = parseQ l'

q_ :: JSONV -> B.ByteString -> JSONV
q_ val l = case q val l of
                Left e -> error e
                Right v -> v

-- | Takes a value and a query. Applies the first selector
-- and returns the remaining query and the selected value.
decompose :: JSONV -> JSONQ -> Maybe (JSONQ, JSONV)
decompose v [] = Just ([],v)
decompose v (s:ss) = case (s,v) of
                        (Key k, JSObject m) -> g_map k m
                        (Idx i, JSArray a) -> g_ary i a
                        _ -> Nothing
    where
        g_map k m = case M.member k m of
                        True -> Just (ss, fromJust $ M.lookup k m)
                        False -> Nothing
        g_ary i a = case i < length a of
                        True -> Just (ss, a !! i)
                        False -> Nothing

run :: JSONV -> JSONQ -> Either String JSONV
run v qry = case decompose v qry of
            (Just ([],   v')) -> Right $ v'
            (Just (rest, v')) -> run v' rest
            Nothing -> Left $ "Unable to find " ++ (show qry)

{- Test data -}

{-
input :: B.ByteString
input = "{ \"the name\": \"john\", \"age\": 24, \"pets\": [ { \"name\": \"Malcolm\", \"type\": \"cat\" }, { \"name\": \"River\", \"type\": \"cat\" }, { \"name\": \"Coach\", \"type\": \"dog\" } ] } "

input2 = "{\"\": \"blank\"}"

query1, query2 :: B.ByteString
query1 = "age"
query2 = "pets[2]"

{- An example using this data:
 - jsonv_ input `q` "pets[0].name"
 - > Right (JSString {fromJSString = "Malcolm"})
 -}

-}
