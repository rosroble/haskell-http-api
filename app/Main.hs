import ApiType
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

main :: IO ()
main = run 8083 kvapp