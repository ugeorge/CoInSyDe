module CoInSyDe.Core.Ginger where

-- import CoInSyDe.Frontend (FrontendException)
import Text.Ginger
import Control.Monad.Identity (Identity, runIdentity)

type TTm = Template SourcePos

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

parseTTm = either (error . show) id . runIdentity . parseGinger nullResolver Nothing
