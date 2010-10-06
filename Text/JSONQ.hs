{-# LANGUAGE OverloadedStrings #-}
module Text.JSONQ (
    JSONV, JSONQ, JSONS,
    jsonv, jsonv_,
    parseQ, parseQ_, check,
    showJSON, showQuery,
    query, query_,
) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- import Text.Regex.Posix -- To be added
import Text.JSON.AttoJSON
import Text.Parsec
import Text.Parsec.ByteString

import System.IO.Unsafe

-- | A JSONV (value) is a synonym for Text.JSON.AttoJSON.JSValue
type JSONV = JSValue

-- | A JSONQ (query) is just a list of selectors.
type JSONQ = [JSONS]

-- | A JSONS (selector)
data JSONS = Key B.ByteString
           | Pat B.ByteString
           | Idx Int
    deriving (Show)

-- | Convert a string to either a JSONS or a parse error.
jsonv :: B.ByteString -> Either String JSONV
jsonv s = parseJSON s

-- | Convert a string to a JSONS. If there is a parse
-- error, an IO error will be raised.
jsonv_ :: B.ByteString -> JSONV
jsonv_ s = case jsonv s of
                Right j -> j
                Left e -> error e

-- | Renders a query back to a string that would
-- reproduce the query when parsed.
showQuery :: JSONQ -> String
showQuery [] = ""
showQuery (q:rest) = 
    let c = one q
        r = map dot rest
    in c ++ concat r
    where
        one q' = case q' of
                    (Key k) -> "'" ++ B.unpack k ++ "'"
                    (Idx i) -> "[" ++ show i ++ "]"
                    (Pat p) -> "<" ++ B.unpack p ++ ">"
        dot q' = case q' of
                    (Idx _) -> one q'
                    _       -> "." ++ one q'

-- | Tries to parse a query. True if valid, False if broken.
check :: B.ByteString -> Bool
check s = case parseQ s of
            Left _ -> False
            Right _ -> True

{- query and query_ only support exact paths into the tree -}

-- | Run a raw query string against a JSONV.
query :: JSONV -> B.ByteString -> Either String JSONV
query v l' = runQuery v l
    where Right l = parseQ l'

-- | Run a raw query string against a JSONV. Errors
-- are raised in IO.
query_ :: JSONV -> B.ByteString -> JSONV
query_ val l = case query val l of
                    Left e -> error e
                    Right v -> v

queryMany :: JSONV -> B.ByteString -> Either String [JSONV]
queryMany = undefined

queryMany_ :: JSONV -> B.ByteString -> [JSONV]
queryMany_ val l = case queryMany val l of
                            Left e -> error e
                            Right v -> v

{- Non-exported functions. -}

-- | Parse a JSONQ from an input string.
parseJSONQ :: Parser JSONQ
parseJSONQ = do
    qry <- parseGroup `sepBy1` (char '.')
    eof
    
    return $ concat qry

-- | Parse a key and 0 to many indicies.
parseGroup :: Parser [JSONS]
parseGroup = do
    k <- choice [parsePat,parseKey]
    i <- many $ parseIdx

    return (k : i)

parsePat :: Parser JSONS
parsePat = do
    _ <- char '<'
    pat <- many1 $ noneOf ">"
    _ <- char '>'
    return $ Pat $ B.pack pat

-- | Parse a key
parseKey :: Parser JSONS
parseKey = try parseQuotedKey
       <|> try parseNormKey

-- | Parse an unquoted, alpha-numeric, key
parseNormKey :: Parser JSONS
parseNormKey = many1 alphaNum >>= return . Key . B.pack

-- | Parse a quoted string.
parseQuotedKey :: Parser JSONS
parseQuotedKey = do
    _ <- char '\''
    k <- many (noneOf "\\'")
    _ <- char '\''
    return . Key . B.pack $ k

-- | Parse an index
parseIdx :: Parser JSONS
parseIdx = do
    _ <- char '['
    d <- many1 digit
    _ <- char ']'

    return . Idx . read $ d

-- | Shorthand to parse a query
parseQ :: B.ByteString -> Either ParseError JSONQ
parseQ = parse parseJSONQ "json-query"

parseQ_ :: B.ByteString -> JSONQ
parseQ_ s = case parseQ s of
                (Left e) -> error $ show e
                (Right q) -> q

-- | Takes a value and a query. Applies the first selector
-- and returns the remaining query and the selected value.
decompose :: JSONV -> JSONQ -> Maybe (JSONQ, JSONV)
decompose v [] = Just ([],v)
decompose v (s:ss) = case (s,v) of
                        (Key k, JSObject m) -> [g_map k m]
                        (Pat p, JSObject m) -> extractMatch p m
                        (Idx i, JSArray a)  -> [g_ary i a]
    where
        g_map k m = case M.member k m of
                        True -> Just (ss, fromJust $ M.lookup k m)
                        False -> Nothing
        g_ary i a = case i < length a of
                        True -> Just (ss, a !! i)
                        False -> Nothing

extractMatch :: B.ByteString -> (M.Map B.ByteString JSValue) -> Maybe (JSONQ, [JSONV])
extractMatch p m = undefined

-- | Run a query against a JSONV.
runQuery :: JSONV -> JSONQ -> Either String JSONV
runQuery v qry = case decompose v qry of
                    (Just ([],   v')) -> Right $ v'
                    (Just (rest, v')) -> runQuery v' rest
                    Nothing -> Left $ "Unable to find " ++ (show qry)

{- Test data -}

input :: B.ByteString
input = unsafePerformIO $ B.readFile "sw17ch.comments.json"

{- An example using this data:
 - jsonv_ input `q` "pets[0].name"
 - > Right (JSString {fromJSString = "Malcolm"})
 -}

