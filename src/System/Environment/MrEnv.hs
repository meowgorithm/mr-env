{-|
Module      : System.Environment.MrEnv
Description : Read environment variables, with default fallbacks
Copyright   : 2020 Christian Rocha
License     : MIT
Maintainer  : christian@rocha.is
Stability   : experimental
Portability : POSIX

A simple way to read environment variables.
-}

module System.Environment.MrEnv (
{-|
Read environment variables with fallback values.

A simple example with @do@ notation:

@
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )

main :: IO ()
main = do

    -- Get a string, with a fallback value if nothing is set.
    host <- envAsString \"HOST\" "localhost"

    -- Get an int. If you need an integer instead you could also use envAsInteger.
    port <- envAsInt \"PORT\" 8000

    -- Get a boolean. Here we're expecting the environment variable to reading
    -- something along the lines of "true", \"TRUE\", \"True\", "truE" and so on.
    debug <- envAsBool \"DEBUG\" False

    putStrLn $
        "Let's connect to "
        ++ host
        ++ " on port "
        ++ show port
        ++ ". Debug mode is "
        ++ if debug then "on" else "off"
        ++ "."
@

You can also read into a record:

@
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )

data Config =
    Config { host  :: String
           , port  :: Int
           , debug :: Bool
           }

getConfig :: IO Config
getConfig = Config
    \<$\> envAsString \"HOST\" "localhost"
    \<*\> envAsInt \"PORT\" 8000
    \<*\> envAsBool \"DEBUG\" False

main :: IO ()
main =
    getConfig >>= \conf ->
        putStrLn $
            "Let's connect to "
            ++ host conf
            ++ " on port "
            ++ show $ port conf
            ++ ". Debug mode is "
            ++ if debug conf then "on" else "off"
            ++ "."
@
-}

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
envAsString :: String
            -- ^Name of environment varaiable
            -> String
            -- ^Fallback value
            -> IO String
            -- ^Result
envAsString name defaultValue =
    (try $ getEnv name :: IO (Either IOError String)) >>= choice
    where
        choice (Left _)    = return defaultValue
        choice (Right val) = return val


{-| Get an environment variable as an int, with a default fallback value -}
envAsInt :: String
         -- ^Name of environment variable
         -> Int
         -- ^Fallback value
         -> IO Int
         -- ^Result
envAsInt name defaultValue =
    envAsString name "" >>= choice
    where
        choice v
          | v == ""   = return defaultValue
          | otherwise = (readMaybe v :: Maybe Int) & fromMaybe defaultValue & return


{-| Get an environment variable as an integer, with a default fallback value -}
envAsInteger :: String
             -- ^Name of environment variable
             -> Integer
             -- ^Fallback value
             -> IO Integer
             -- ^Result
envAsInteger name defaultValue =
    envAsString name "" >>= choice
    where
        choice v | v == ""   = return defaultValue
                 | otherwise = (readMaybe v :: Maybe Integer) & fromMaybe defaultValue & return


{-| Get an environment variable as a boolean, with a default fallback value -}
envAsBool :: String
          -- ^Name of environment variable
          -> Bool
          -- ^Fallback value
          -> IO Bool
          -- ^Result
envAsBool name defaultValue =
    envAsString name "" >>= \val ->
        if val == ""
           then return defaultValue
           else (readMaybe $ capitalize val :: Maybe Bool)
                & fromMaybe defaultValue
                & return


{-| Capitalize the first character in a string and make all other characters
    lowercase. In our case we're doing this so values like like  TRUE, true,
    True, and truE all become "True," which can then be coerced to a boolean. -}
capitalize :: String -> String
capitalize [] = []
capitalize (head':tail') =
    Char.toUpper head' : map Char.toLower tail'
