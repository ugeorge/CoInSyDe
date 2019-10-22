{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module CoInSyDe.Backend.C where

import CoInSyDe.Core
import CoInSyDe.Frontend
import Data.Text (Text,pack,unpack,append,cons,snoc)
import Data.Text.Read
import Data.Map.Lazy as M hiding (map,fold,filter)

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
  data Glue C = Variable {glName :: Text, glTy :: Type C, varVal   :: Maybe Text}
              | State    {glName :: Text, glTy :: Type C, stateVal :: Text} 
              | InArg    {glName :: Text, glTy :: Type C, argVal   :: Maybe Text}
              | RetArg   {glName :: Text, glTy :: Type C} 
              deriving (Read, Show, Eq)
                       
  mkType _ typeLib node =
    case node @! "class" of
      "primitive" -> mkPrimTy targetName
      "bool"   -> mkBoolTy targetName parameters
      "enum"   -> mkEnumTy targetName parameters
      "struct" -> mkStruct typeLib targetName parameters
      "array"  -> mkArray  typeLib targetName parameters
      -- "circular_buffer" -> mkCircBuf targetName parameters
      x        -> error $ "Type class " ++ show x ++ " is not recognized!"
    where targetName = node @! "targetName"
          parameters = node |= "parameter"
          
  mkGlue pId typeLib node =
    case node @! "class" of
      "input"    -> mkInArg typeLib node
      "output"   -> mkRetArg typeLib node
      "variable" -> mkVar typeLib node
      "state"    -> mkState typeLib pId node
      x          -> error $ "Port class " ++ show x ++ " is not recognized!"
      

------ TYPE CONSTRUCTORS ------

mkPrimTy tName = PrimTy tName
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

isInput InArg{}       = True
isInput _             = False
isOutput RetArg{}     = True
isOutput _            = False
isState State{}       = True
isState _             = False
isVariable Variable{} = True
isVariable _          = False

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

------ SPECIFIC COMPILER CHAIN STAGES ------

buildStateVars :: FNode n => Dict (Type C) -> n -> PortMap C
buildStateVars tyLib root = M.fromList states
  where
    pNodes = concatMap groupByName $ childrenOf ["pattern","composite"] root
    states = map mkStateVar $ filter (hasValue "class" "state" . snd) pNodes
    -----------------------------------
    groupByName n = map (\p-> (n @! "name", p)) (n |= "port")
    mkStateVar (parentId, n) = let stVar = mkState tyLib parentId n
                               in (glName stVar, stVar)

    
-- getInitVal node = case node |= "parameter" of
--                     []  -> Nothing
--                     [n] -> getParamVal "initValue" n
--                     _   -> error $ "Node " ++ show (node @! "name")
--                            ++ "cannot have multiple initial values!"
