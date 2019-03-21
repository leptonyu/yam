# yam

| package name | version |
|-|-|
| yam |[![Hackage](https://img.shields.io/hackage/v/yam.svg)](https://hackage.haskell.org/package/yam)|
| yam-datasource |[![Hackage](https://img.shields.io/hackage/v/yam-datasource.svg)](https://hackage.haskell.org/package/yam-datasource)|

[![stackage LTS package](http://stackage.org/package/yam/badge/lts)](http://stackage.org/lts/package/yam)
[![stackage Nightly package](http://stackage.org/package/yam/badge/nightly)](http://stackage.org/nightly/package/yam)
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
