module Text.JSONQ (
    JSONQ,
) where

import Text.JSON
import Text.JSON.String

newtype JSONQ = JSONQ JSValue

instance Show JSONQ where
    show (JSONQ jsv) = "JSONQ << " ++ (showJSTopType jsv) "" ++ " >>"

-- | Convert a string to either a JSONQ or a parse error.
jsonq :: String -> Either String JSONQ
jsonq s = case runGetJSON readJSTopType s of
                Right j -> Right $ JSONQ j
                Left e  -> Left e

-- | Convert a string to a JSONQ. If there is a parse
-- error, an IO error will be raised.
jsonq_ :: String -> JSONQ
jsonq_ s = case jsonq s of
                Right j -> j
                Left e -> error e
