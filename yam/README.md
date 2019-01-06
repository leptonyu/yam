# yam

Servant based Web Wrapper for Production in Haskell.


```Haskell

import qualified Data.Salak                     as S
import           Yam

type API = "hello" :> Get '[PlainText] Text

service :: ServerT API App
service = return "world"

main = start S.empty [] (Proxy :: Proxy API) service


```
