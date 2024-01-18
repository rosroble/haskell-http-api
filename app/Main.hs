import ApiType
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import Data.IORef
import Data.Map
import ApiType
import Options.Applicative
import Servant.TypeScript


data Options = Options
  { generate :: Bool
  } deriving Show

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch ( long "generate"
               <> help "Generate TypeScript client libraries for Servant"
               )

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Haskell servant API for key-value storage. Functional programming, ITMO 2023."
            <> header "Haskell servant API"
        )

runWithOptions :: Options -> IO ()
runWithOptions (Options generate) = do
  if generate
    then generateClientLibraries
    else runServer


generateClientLibraries :: IO()
generateClientLibraries = do
    writeTypeScriptLibrary (Proxy :: Proxy KVAPI) "./apigen/"

runServer :: IO ()
runServer = do
    ref <- newIORef (Data.Map.empty :: Map KeyValueType KeyValueType)
    run 8083 (kvapp ref)