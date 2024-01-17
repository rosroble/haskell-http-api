import ApiType
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import Data.IORef
import Data.Map
import ApiType

main :: IO ()
main = do
    ref <- newIORef (empty :: Map KeyValueType KeyValueType)
    -- run 8083 (app ref)
    run 8083 (kvapp ref)