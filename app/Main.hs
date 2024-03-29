import ApiType
import Data.IORef
import Data.Map
import Docs (docAsString)
import Network.Wai.Handler.Warp
import Options.Applicative
import Servant
import Servant.TypeScript

data Options = Options
  { generateClient :: Bool,
    generateApiDocs :: Bool
  }
  deriving (Show)

generateClientOption :: Parser Bool
generateClientOption =
  switch
    ( long "generate-client"
        <> help "Generate TypeScript client libraries for Servant"
    )

generateApiDocsOption :: Parser Bool
generateApiDocsOption =
  switch
    ( long "generate-docs"
        <> help "Generate API docs"
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> generateClientOption
    <*> generateApiDocsOption

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
runWithOptions (Options genclient gendocs) = do
  if genclient
    then generateClientLibraries
    else
      if gendocs
        then writeFile "api-docs.md" (docAsString kvAPI)
        else runServer

generateClientLibraries :: IO ()
generateClientLibraries = do
  writeTypeScriptLibrary (Proxy :: Proxy KVAPI) "./apigen/"

runServer :: IO ()
runServer = do
  ref <- newIORef (Data.Map.empty :: Map KeyValueType KeyValueType)
  run 8083 (kvapp ref)
