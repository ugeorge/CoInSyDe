module CoInSyDe.Backend.C.Gen where

import Data.Text as T
import Data.Map.Lazy as M

import CoInSyDe.Dictionary
import CoInSyDe.Core
import CoInSyDe.Backend.C.Core

-- TODO: Monad instance; monadic generation during a single traversal.
data Proj = Proj { welcome  :: Text
                 , top      :: Id
                 , funDecls :: [Id]
                 , requmnts :: [Requ C]
                 , globVars :: IfMap C
                 , types    :: Dict (Type C)
                 , funcs    :: Dict (Comp C)
                 } deriving (Show)

emptyProj = Proj T.empty T.empty [] [] M.empty emptyDict emptyDict

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //\n\n"



-- traverse :: Dict (Type C) -> Dict (Comp C) -> Id -> Gen

