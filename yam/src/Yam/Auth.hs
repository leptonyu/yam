module Yam.Auth where
import           Control.Lens
import           Data.Default
import           Data.Swagger
import qualified Data.Vault.Lazy                            as L
import           Servant.Server.Internal.RoutingApplication
import           Servant.Swagger
import           Servant.Swagger.Internal
import           System.IO.Unsafe                           (unsafePerformIO)
import           Yam.Internal                               hiding (name)

data CheckAuth (principal :: *)

newtype AuthChecker principal = AuthChecker { runCheckAuth :: Request -> App principal }

instance Default (AuthChecker principal) where
  def = AuthChecker $ \_ -> throwS err401 "No Auth Checker"

class HasAuthKey principal where
  authKey :: Key (AuthChecker principal)
  authKey = unsafePerformIO newKey
  toLog :: principal -> Text

instance ( HasContextEntry context Env
         , HasServer api context
         , HasAuthKey principal)
         => HasServer (CheckAuth principal :> api) context where
  type ServerT (CheckAuth principal :> api) m = principal -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  route _ context server = route (Proxy :: Proxy api) context $ server `addFixAuthCheck` authCheck
    where
      env :: Env
      env = getContextEntry context
      checker :: Request -> App principal
      checker = runCheckAuth $ reqAttr authKey env
      authCheck :: DelayedIO principal
      authCheck = withRequest $ \req -> liftIO $ runAppM env { reqAttributes = Just (vault req)} (checker req)
      -- Fix Origin `addAuthCheck`, add logger info
      addFixAuthCheck
        :: Delayed env (principal -> b)
        -> DelayedIO principal
        -> Delayed env b
      addFixAuthCheck Delayed{..} new =
        Delayed
          { authD   = (,) <$> authD <*> new
          , serverD = \ c p h (y, v) b req -> ($ v) <$> serverD c p h y b req { vault = L.adjust (\x -> x <> "," <> toLog v) extensionLogKey $ vault req}
          , ..
          }

instance (HasSwagger api, ToParamSchema principal) => HasSwagger (CheckAuth principal :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & addParam param
    where
      param = mempty
        & Data.Swagger.name .~ "Authorization"
        & required ?~ True
        & schema .~ ParamOther (mempty
            & in_ .~ ParamHeader
            & paramSchema .~ toParamSchema (Proxy :: Proxy principal))

authAppMiddleware :: HasAuthKey principal => AuthChecker principal -> AppMiddleware
authAppMiddleware = simpleAppMiddleware (True, "Auth Plugin") authKey
