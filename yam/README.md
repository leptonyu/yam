# yam

| package name | version |
|-|-|
| yam |[![Hackage](https://img.shields.io/hackage/v/yam.svg)](https://hackage.haskell.org/package/yam)|
| yam-datasource |[![Hackage](https://img.shields.io/hackage/v/yam-datasource.svg)](https://hackage.haskell.org/package/yam-datasource)|
| yam-redis |[![Hackage](https://img.shields.io/hackage/v/yam-redis.svg)](https://hackage.haskell.org/package/yam-redis)|

[![stackage LTS package](http://stackage.org/package/yam/badge/lts)](http://stackage.org/lts/package/yam)
[![stackage Nightly package](http://stackage.org/package/yam/badge/nightly)](http://stackage.org/nightly/package/yam)
[![Build Status](https://travis-ci.org/leptonyu/yam.svg?branch=master)](https://travis-ci.org/leptonyu/yam)

Servant based Web Wrapper for Production in Haskell.


```Haskell

import           Salak
import           Salak.Yaml
import           Servant
import           Yam
import qualified Control.Category    as C
import           Data.Version

type API = "hello" :> Get '[PlainText] Text

service :: ServerT API AppSimple
service = return "world"

main = runSalakWith "app" YAML $ do
  al <- require  "yam.application"
  sw <- require  "yam.swagger"
  lc <- requireD "yam.logging"
  start al sw (makeVersion []) lc spanNoNotifier emptyAM serveWarp (Proxy @API) service

```
