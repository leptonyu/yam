module Data.Opentracing.Types where

import qualified Data.HashMap.Lazy as HM
import           Data.Scientific
import           Data.Text         (Text)

data SpanTag
  = TagString Text
  | TagBool   Bool
  | TagNum    Scientific
  deriving (Eq, Show)

data SpanContext = SpanContext
  { traceId :: Text
  , baggage :: HM.HashMap Text (Maybe Text)
  } deriving (Eq, Show)

data SpanReferenceType
  = FollowsFrom
  | ChildOf
  deriving (Eq, Show)

data SpanReference = SpanReference
  { referenceType :: SpanReferenceType
  , parentId      :: Text
  } deriving (Eq, Show)

data Span = Span
  { spanId     :: Text
  , name       :: Text
  , startTime  :: Int
  , finishTime :: Maybe Int
  , tags       :: HM.HashMap Text SpanTag
  , logs       :: HM.HashMap Text Text
  , context    :: SpanContext
  , references :: [SpanReference]
  } deriving (Eq, Show)
