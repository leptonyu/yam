module Yam.Internal.Types where

import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import qualified Data.Vault.Lazy                as L
import           Salak
import           Servant
import           Yam.Internal.Config

class HasCxt cxt c where
  getCxt :: cxt -> c

instance HasCxt c c where
  getCxt = id

askCxt :: (Monad m, HasCxt cxt a, MonadReader cxt m) => m a
askCxt = getCxt <$> ask

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Holder for 'LogFunc'
newtype LogFuncHolder = LogFuncHolder LogFunc
-- | Holder for 'Vault'
newtype VaultHolder   = VaultHolder (Maybe L.Vault)


data BaseCxt = BaseCxt
  { salakCxt  :: SourcePack
  , loggerCxt :: LogFuncHolder
  , vaultCxt  :: VaultHolder
  , appCxt    :: AppConfig
  }

instance HasCxt BaseCxt SourcePack where
  getCxt = salakCxt

instance HasCxt BaseCxt LogFuncHolder where
  getCxt = loggerCxt

instance HasCxt BaseCxt VaultHolder where
  getCxt = vaultCxt

instance HasCxt BaseCxt AppConfig where
  getCxt = appCxt

instance {-# OVERLAPPING #-} (HasCxt BaseCxt a, HasContextEntry cxt BaseCxt) => HasCxt (Context cxt) a where
  getCxt = getCxt @BaseCxt . getContextEntry

type HasConfig cxt = HasCxt cxt AppConfig
type HasSalak  cxt = HasCxt cxt SourcePack
type HasLogger cxt = (HasCxt cxt LogFuncHolder, HasCxt cxt VaultHolder)
type HasBase   cxt = (HasLogger cxt, HasSalak cxt, HasConfig cxt)

