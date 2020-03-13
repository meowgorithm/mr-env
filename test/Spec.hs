import Test.Hspec
import System.Environment ( setEnv )
import System.Environment.MrEnv

data Config =
    Config { host  :: String
           , port  :: Int
           , cats  :: Integer
           , debug :: Bool
           }

getConfig :: IO Config
getConfig = Config
    <$> envAsString "HOST" "localhost"
    <*> envAsInt "PORT" 8000
    <*> envAsInteger "CATS" 25
    <*> envAsBool "DEBUG" False

setEnvironment :: IO ()
setEnvironment =
       setEnv "HOST" "purr.cat"
    >> setEnv "PORT" "80"
    >> setEnv "CATS" "3"
    >> setEnv "DEBUG" "true"

main :: IO ()
main =
    hspec $ do
        describe "Mr. Env's behavior" $ do

            it "uses default fallback variables" $ do
                c <- getConfig
                host c `shouldBe` "localhost"
                port c `shouldBe` 8000
                cats c `shouldBe` 25
                debug c `shouldBe` False

            it "reads variables from the environment" $ do
                setEnvironment
                c <- getConfig
                host c `shouldBe` "purr.cat"
                port c `shouldBe` 80
                cats c `shouldBe` 3
                debug c `shouldBe` True
