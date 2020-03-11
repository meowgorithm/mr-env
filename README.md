# Mr. Env

Read values from the environment with Haskell.

```haskell
import System.Environment.MrEnv ( envAsBool, envAsInt, envAsInteger, envAsString )

main :: IO ()
main = do

    -- Get a string, with a fallback value if nothing is set
    host <- envAsString "HOST" "localhost"

    -- Get a int. If you need an integer instead you could also use envAsInteger.
    port <- envAsInt "PORT" 8000

    -- Get a boolean
    debug <- envAsBool "DEBUG" False

    putStrLn $ "Let's connect to " ++ host ++ " on port " ++ show port ++ ". "
        ++ "Debug mode is " ++ (if debug then "on" else "off") ++ "."
```

## Author

[Christian Rocha](https://github.com/meowgorithm)

## License

MIT
