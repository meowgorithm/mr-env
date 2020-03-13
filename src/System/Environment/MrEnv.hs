{-# LANGUAGE LambdaCase #-}

module System.Environment.MrEnv (
        envAsBool
      , envAsInt
      , envAsInteger
      , envAsString ) where

import Control.Exception ( try )
import System.Environment ( getEnv )
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe )
import Data.Function ( (&) )
import qualified Data.Char as Char


{-| Get an environment variable as a string, with a default fallback value -}
envAsString :: String -> String -> IO String
envAsString name defaultValue =
    (try $ getEnv name :: IO (Either IOError String)) >>= \case
        Left _ ->
            return defaultValue
        Right val ->
            return val


{-| Get an environment variable as an int, with a default fallback value -}
envAsInt :: String -> Int -> IO Int
envAsInt name defaultValue =
    envAsString name "" >>= \val ->
        if val == ""
            then return defaultValue
            else return $
                (readMaybe val :: Maybe Int) & fromMaybe defaultValue


{-| Get an environment variable as an integer, with a default fallback value -}
envAsInteger :: String -> Integer -> IO Integer
envAsInteger name defaultValue =
    envAsInt name (fromInteger defaultValue) >>= \val ->
        return $ toInteger val


{-| Get an environment variable as a boolean, with a default fallback value -}
envAsBool :: String -> Bool -> IO Bool
envAsBool name defaultValue =
    envAsString name "" >>= \val ->
        if val == ""
           then return defaultValue
           else return $
               let s = capitalize val in
               (readMaybe s :: Maybe Bool) & fromMaybe defaultValue


{-| Capitalize the first character in a string -}
capitalize :: String -> String
capitalize [] = []
capitalize (head':tail') =
    Char.toUpper head' : map Char.toLower tail'
