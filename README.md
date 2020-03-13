# Mr. Env

Read values from the environment with Haskell.

## Simple Example

```haskell
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )

main :: IO ()
main = do

    -- Get a string, with a fallback value if nothing is set.
    host <- envAsString "HOST" "localhost"

    -- Get an int. If you need an integer instead you could also use envAsInteger.
    port <- envAsInt "PORT" 8000

    -- Get a boolean. Here we're expecting the environment variable to reading
    -- something along the lines of "true", "TRUE", "True", "truE" and so on.
    debug <- envAsBool "DEBUG" False

    putStrLn $
        "Let's connect to "
        ++ host
        ++ " on port "
        ++ show port
        ++ ". Debug mode is "
        ++ if debug then "on" else "off"
        ++ "."
```

## Fancy Example

```haskell
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )

data Config =
    Config { host  :: String
           , port  :: Int
           , debug :: Bool
           }

getConfig :: IO Config
getConfig = Config
    <$> envAsString "HOST" "localhost"
    <*> envAsInt "PORT" 8000
    <*> envAsBool "DEBUG" False

main :: IO ()
main =
    getConfig >>= \conf ->
        putStrLn $
            "Let's connect to "
            ++ host c
            ++ " on port "
            ++ show $ port c
            ++ ". Debug mode is "
            ++ if debug c then "on" else "off"
            ++ "."
```

## Author

[Christian Rocha](https://github.com/meowgorithm)

## License

MIT
