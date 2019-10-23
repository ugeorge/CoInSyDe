{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module CoInSyDe.Backend.C.Core where

import CoInSyDe.Core
import CoInSyDe.Frontend
import Data.List (nub)
import Data.Text (Text,append,snoc)
import Data.Text.Read
import Data.Map.Lazy as M hiding (map,filter)

data C = C

instance Target C where
  data Type C = PrimTy  {tyName :: Id}
              | BoolTy  {tyName :: Id, boolTrue :: Text, boolFalse :: Text}
              | EnumTy  {tyName :: Id, enumVals :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries :: Map Text (Type C, Maybe Text)}
              | Array   {tyName :: Id, arrBaseTy :: Type C, arrSize :: Int}
              | CircBuf {tyName :: Id, bufArr :: Type C,  bufPos :: Int}
              | NoTy
              deriving (Read, Show, Eq)                       
  mkType _ typeLib node =
    case node @! "class" of
      "primitive" -> mkPrimTy targetName
      "bool"   -> mkBoolTy targetName parameters
      "enum"   -> mkEnumTy targetName parameters
      "struct" -> mkStruct typeLib targetName parameters
      "array"  -> mkArray  typeLib targetName parameters
      -- "circular_buffer" -> mkCircBuf targetName parameters
      x -> error $ "Type class " ++ show x ++ " is not recognized!"
    where targetName = node @! "targetName"
          parameters = node |= "parameter"
          
  data Glue C = Param   {glName :: Text, paramVal :: Text}
              | LocVar  {glName :: Text, glTy :: Type C, varVal   :: Maybe Text}
              | GlobVar {glName :: Text, glTy :: Type C, stateVal :: Text} 
              | InArg   {glName :: Text, glTy :: Type C, argVal   :: Maybe Text}
              | RetArg  {glName :: Text, glTy :: Type C} 
              | Get     {glName :: Text, glTy :: Type C} 
              | Put     {glName :: Text, glTy :: Type C} 
              deriving (Read, Show, Eq)
  mkGlue pId typeLib node =
    case (getName node, node @? "dir", node @! "class") of
      ("port",Just "in", "arg") -> mkInArg typeLib node
      ("port",Just "out","arg") -> mkRetArg typeLib node
      ("port",Just "in",  _)    -> mkGet typeLib node
      ("port",Just "out", _)    -> mkPut typeLib node
      ("internal",_,"var")      -> mkVar typeLib node
      ("internal",_,"state")    -> mkState typeLib pId node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"

  data Requ C = Include Text deriving (Read, Show, Eq)
  mkRequ node = Include (node @! "include")

------ TYPE CONSTRUCTORS ------

mkPrimTy = PrimTy
mkBoolTy tName pNodes = BoolTy tName true false
  where true  = getParam "true" pNodes
        false = getParam "false" pNodes
mkEnumTy tName pNodes = EnumTy tName (map extract pNodes)
  where extract n = (n @! "name", n @? "value")
mkStruct tyLib tName pNodes = Struct tName (M.fromList $ map extract pNodes)
  where extract n = (n @! "name", (tyLib ! (n @! "type"), n @? "value"))
mkArray tyLib tName pNodes = Array tName baseTy size
  where baseTy  = tyLib ! getParam "baseType" pNodes
        size    = fst $ either error id $ decimal $ getParam "size" pNodes

getParam name nodes = head (filterByAttr "name" name nodes) @! "value"

------ GLUE CONSTRUCTORS ------

mkInArg tyLib node = InArg name ty val
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")
        val  = node @? "value"
mkRetArg tyLib node = RetArg name ty
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")
mkVar tyLib node = Variable name ty val
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")
        val  = node @? "value"
mkState tyLib parentId node = State name ty val
  where name = (parentId `snoc` '_') `append` (node @! "name")
        ty   = tyLib ! (node @! "type")
        val  = node @! "value"

isInput InArg{}       = True
isInput _             = False
isOutput RetArg{}     = True
isOutput _            = False
isState GlobVar{}     = True
isState _             = False
isVariable LocVar{}   = True
isVariable _          = False

------ SPECIFIC COMPILER CHAIN DICTIONARIES ------

mkStateDict :: FNode n => Dict (Type C) -> n -> PortMap C
mkStateDict tyLib root = M.fromList states
  where
    pNodes = concatMap groupByName $ childrenOf ["pattern","composite"] root
    states = map mkStateVar $ filter (hasValue "class" "state" . snd) pNodes
    -----------------------------------
    groupByName n = map (\p-> (n @! "name", p)) (n |= "port")
    mkStateVar (parentId, n) = let stVar = mkState tyLib parentId n
                               in (glName stVar, stVar)

mkRequList :: Dict (Fun C) -> [Requ C]
mkRequList = nub . concatMap (requires . snd) . M.toList

-- getInitVal node = case node |= "parameter" of
--                     []  -> Nothing
--                     [n] -> getParamVal "initValue" n
--                     _   -> error $ "Node " ++ show (node @! "name")
--                            ++ "cannot have multiple initial values!"

