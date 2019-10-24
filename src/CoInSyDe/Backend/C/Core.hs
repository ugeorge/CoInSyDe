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
              -- | CircBuf {tyName :: Id, bufArr :: Type C,  bufPos :: Int}
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
          
  data Glue C = Param   {glName :: Text, glTy :: Type C, paramVal :: Text}
              | LocVar  {glName :: Text, glTy :: Type C, glVal    :: Maybe Text}
              | GlobVar {glName :: Text, glTy :: Type C, stateVal :: Text} 
              | InArg   {glName :: Text, glTy :: Type C, glVal    :: Maybe Text}
              | RetArg  {glName :: Text, glTy :: Type C} 
              | Get     {glName :: Text, glTy :: Type C, glVal    :: Maybe Text} 
              | Put     {glName :: Text, glTy :: Type C, glVal    :: Maybe Text} 
              deriving (Read, Show)
  mkGlue pId typeLib node =
    case (getName node, node @? "class") of
      ("port",Just "iArg")     -> mkInArg typeLib node
      ("port",Just "oArg")     -> mkRetArg typeLib node
      ("port",Just "get")      -> mkGet typeLib node
      ("port",Just "put")      -> mkPut typeLib node
      ("intern", Just "var")   -> mkVar typeLib node
      ("intern", Just "state") -> mkState typeLib pId node
      ("parameter",_)          -> mkParam typeLib node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"

  data Requ C = Include Text deriving (Read, Show, Eq)
  mkRequ node = Include (node @! "include")

------ TYPE CONSTRUCTORS ------

-- | Makes a 'PrimTy' from a node
--
-- > type[@class="primitive",@targetName=*]
mkPrimTy = PrimTy

-- | Makes a 'BoolTy' from a node
--
-- > type[@class="bool",@targetName=*]
-- > - parameter[@name="true",@value=*]
-- > - parameter[@name="false",@value=*]
mkBoolTy tName pNodes = BoolTy tName true false
  where true  = getParam "true" pNodes
        false = getParam "false" pNodes

-- | Makes a 'EnumTy' from a node
--
-- > type[@class="enum",@targetName=*]
-- > + parameter[@name=*,@?value=*]
mkEnumTy tName pNodes = EnumTy tName (map extract pNodes)
  where extract n = (n @! "name", n @? "value")

-- | Makes a 'Struct' from a node
--
-- > type[@class="struct",@targetName=*]
-- > + parameter[@name=*,@type=*,@?value=*]
mkStruct tyLib tName pNodes = Struct tName (M.fromList $ map extract pNodes)
  where extract n = (n @! "name", (tyLib ! (n @! "type"), n @? "value"))

-- | Makes an 'Array' from a node
--
-- > type[@class="array",@targetName=*]
-- > - parameter[@name="baseType",@value=*]
-- > - parameter[@name="size",@value=*]
mkArray tyLib tName pNodes = Array tName baseTy size
  where baseTy  = tyLib ! getParam "baseType" pNodes
        size    = fst $ either error id $ decimal $ getParam "size" pNodes

getParam name nodes = head (filterByAttr "name" name nodes) @! "value"

------ GLUE CONSTRUCTORS ------

-- | Makes an 'InArg' from a node
--
-- > port[@class="iArg",@name=*,@type=*,@?value=*]
mkInArg tyLib node = InArg name ty val
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")
        val  = node @? "value"

-- | Makes a 'RetArg' from a node
--
-- > port[@class="oArg",@name=*,@type=*]
mkRetArg tyLib node = RetArg name ty
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")

-- | Makes a 'Get' from a node
--
-- > port[@class="get",@name=*,@type=*,@?value=*]
--
-- Notice that @class@ needs to point to a functional for a \"get-like\" glue
-- mechanism. This mechanism will be instantiated same as any other template instance,
-- by using bindings defined in a sibling @instance@ node.
mkGet tyLib node = Get name ty ref
  where name = node @! "name"
        ty   = tyLib ! (node @! "type")
        val  = node @? "value"
                       
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

mkStateDict :: FNode n => Dict (Type C) -> n -> GlueMap C
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

