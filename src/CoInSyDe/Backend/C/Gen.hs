module CoInSyDe.Backend.C.Gen where

import Data.Text
import CoInSyDe.Backend.C.Core

-- TODO: Monad instance; monadic generation during a single traversal.
data Generator = Gen { code   :: [Text]
                     , reqs   :: [Requ C]
                     , glVars :: [If C]
                     } deriving (Show)
