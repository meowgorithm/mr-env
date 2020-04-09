# Mr. Env

![build][action-badge] [![Hackage][hackage-shield]][hackage]

[action-badge]: https://github.com/meowgorithm/mr-env/workflows/build/badge.svg
[hackage]: http://hackage.haskell.org/package/mr-env
[hackage-shield]: https://img.shields.io/hackage/v/mr-env.svg?style=flat&color=blueviolet

A simple way to read environment variables in Haskell.

```haskell
-- Read environment variables, with defaults
host <- envAsString "HOST" "localhost"
port <- envAsInt "PORT" 8000
```

## Simple Example

Read environment variables with `do` notation:

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

Read environment variables into a record:

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
            ++ host conf
            ++ " on port "
            ++ show $ port conf
            ++ ". Debug mode is "
            ++ if debug conf then "on" else "off"
            ++ "."
```

We suggest pronouncing `<*>` _[brackety-splat][1]_ (as
opposed to _ap_). In that vein, `<$>` is _brackety-cash._

[1]: https://www.reddit.com/r/haskell/comments/241jcm/how_do_you_say/

## Author

[Christian Rocha](https://github.com/meowgorithm)

## License

MIT
