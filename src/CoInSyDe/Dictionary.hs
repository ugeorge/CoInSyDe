module CoInSyDe.Dictionary where

import Data.Typeable
import Control.DeepSeq
import Data.Text as T (Text)
import Data.Map.Lazy as M hiding (map,foldr,filter)

import CoInSyDe.Frontend

type Id      = Text -- ^ Generic identifier used as library search key
type Name    = Text -- ^ Generic (e.g. variable) name, read from the user input
type Keyword = Text -- ^ A "reserved" keyword used in template identifiers, see 'TTm'

type Dict t = Map Id (t,History)

emptyDict = empty

type History = [Info]

data Info = Info {ldFile :: FilePath, ldInfo :: String} deriving (Show,Read)
instance NFData Info where
  rnf _ = ()


mkInfoNode :: FNode n => FilePath -> n -> Info
mkInfoNode path n = Info path (getInfo n)

infixl 9 !*, !^

(!*) :: Typeable t => Dict t -> Id -> t
(!*) d k = fst $  maybe (error msg) id (d !? k)
  where msg = "ID " ++ show k ++ " does not exist in the current database of type: "
              ++ show (typeOf d)

(!^) :: Typeable t => Dict t -> Id -> History
(!^) d k = snd $  maybe (error msg) id (d !? k)
  where msg = "ID " ++ show k ++ " does not exist in the current database of type: "
              ++ show (typeOf d)

mkDict :: [(Id, t, History)] -> Dict t
mkDict = M.fromList . map (\(i,c,h) -> (i,(c,h)))

dictReplace :: Id -> t -> Info -> Dict t -> Dict t
dictReplace name c info = insertWith f name (c,[info])
  where f (a,newh) (_,oldh) = (a,newh++oldh)

dictKeep :: Id -> t -> Info -> Dict t -> Dict t
dictKeep name c info = insertWith f name (c,[info])
  where f (_,newh) (a,oldh) = (a,oldh++newh)
