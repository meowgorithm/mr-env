{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Exception ( catch )
import Data.Char ( toLower, toUpper )
import Data.Maybe ( fromMaybe )
import System.Environment ( getEnv )
import Text.Read ( readMaybe )



{-| Get an environment variable, with a fallback value and the ability to
   preprocess the raw string before @read@ing it. -}
envAs' :: forall a. Read a
       => String              -- ^Name of environment variable
       -> (String -> Maybe a) -- ^Preprocessing function
       -> a                   -- ^Fallback value
       -> IO a                -- ^Result
envAs' name prep defaultValue =
    catch
    (fromMaybe defaultValue . prep <$> getEnv name)
    ((const $ pure defaultValue) :: IOError -> IO a)


{-| Get an environment variable, with a fallback value -}
envAs :: forall a. Read a
      => String -- ^Name of environment variable
      -> a      -- ^Fallback value
      -> IO a   -- ^Result
envAs name =
    envAs' name readMaybe

{-| Get an environment variable as a @'String'@, with a fallback value. Use
   this instead of @'envAs' \@String@, because 'readMaybe' fails unless your
   'String's are doubly-quoted (i.e. '"\"value\""' -}
envAsString :: String    -- ^Name of environment variable
            -> String    -- ^Fallback value
            -> IO String -- ^Result
envAsString name =
    envAs' name (readMaybe . (\v -> "\"" ++ v ++ "\""))


{-| Get an environment variable as an @'Int'@, with a fallback value -}
envAsInt :: String -- ^Name of environment variable
         -> Int    -- ^Fallback value
         -> IO Int -- ^Result
envAsInt =
    envAs


{-| Get an environment variable as an @'Integer'@, with a fallback value -}
envAsInteger :: String     -- ^Name of environment variable
             -> Integer    -- ^Fallback value
             -> IO Integer -- ^Result
envAsInteger =
    envAs


{-| Get an environment variable as a boolean, with a fallback value. This
   function is recommended over @'envAs' \@Bool@, as it handles nonstandard
   capitalization. -}
envAsBool :: String    -- ^Name of environment variable
            -> Bool    -- ^Fallback value
            -> IO Bool -- ^Result
envAsBool name =
    envAs' name (readMaybe . capitalize)


{-| Capitalize the first character in a string and make all other characters
    lowercase. In our case we're doing this so values like like  TRUE, true,
    True, and truE all become "True," which can then be coerced to a boolean. -}
capitalize :: String -> String
capitalize [] = []
capitalize (head':tail') = toUpper head' : map toLower tail'
