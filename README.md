# yam

| package name | version |
|-|-|
| yam |[![Hackage](https://img.shields.io/badge/hackage-v0.5.4-orange.svg)](https://hackage.haskell.org/package/yam)|
| yam-datasource |[![Hackage](https://img.shields.io/badge/hackage-v0.5.4-orange.svg)](https://hackage.haskell.org/package/yam-datasource)|

[![Build Status](https://travis-ci.org/leptonyu/yam.svg?branch=master)](https://travis-ci.org/leptonyu/yam)

Servant based Web Wrapper for Production in Haskell.


```Haskell

import qualified Data.Salak                     as S
import           Servant
import           Yam

type API = "hello" :> Get '[PlainText] Text

service :: ServerT API App
service = return "world"

main = start S.empty [] (Proxy :: Proxy API) service


```
