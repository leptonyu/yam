# yam

[![Hackage](https://img.shields.io/badge/hackage-v0.5.1-orange.svg)](https://hackage.haskell.org/package/yam)

Servant based Web Wrapper for Production in Haskell.


```Haskell

import qualified Data.Salak                     as S
import           Yam

type API = "hello" :> Get '[PlainText] Text

service :: ServerT API App
service = return "world"

main = start S.empty [] (Proxy :: Proxy API) service


```
