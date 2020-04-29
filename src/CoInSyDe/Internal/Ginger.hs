{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Internal.Ginger where

import           Control.Monad.Writer.Lazy
import           Data.Default
import qualified Data.HashMap.Strict as H
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Ginger
import           Text.Ginger.GVal
import           Text.Ginger.Run.Type (GingerContext (..),throwHere)
import           Text.Ginger.Run.VM

import           CoInSyDe.Internal.YAML
import           CoInSyDe.Internal.Map

------------------------------------------------------------------

-- | Accumulator type for own writer monad to use in Ginger transformer
newtype GAcc =  Acc { getAccum :: Either String Text }

instance Semigroup GAcc where
  (Acc (Right x)) <> (Acc (Right y)) = Acc (Right (mappend x y))
  (Acc (Left x))  <> _ = Acc (Left x)
  _  <> (Acc (Left x)) = Acc (Left x)
  
instance Monoid GAcc where
  mempty = Acc (Right T.empty)

------------------------------------------------------------------

type GWriter  = Writer GAcc
type GRunner  = Run SourcePos GWriter Text
type GContext = GingerContext SourcePos GWriter Text

tellG  = tell . Acc
newG x = writer (T.empty, Acc $ Right x)
execG  = getAccum . execWriter

-- | Makes a template placeholder function from a target function @\ id arglist@. This
-- function uses only positional arguments.
mkPlaceholderFun :: (Text -> [Text] -> Text) -> Function GRunner
mkPlaceholderFun f xs = do
  _ <- liftRun2 newG $ (\(name:args) -> f name args) $ map (asText . snd) xs
  return def

mkGingerContext :: Info      -- ^ where the template source is found
                -> (Text -> [Text] -> Text)
                -- ^ placeholder expander function defined by the target
                -> Map YNode -- ^ built YAML dictionary
                -> GContext  -- ^ Ginger context
mkGingerContext info phFun usrDict = makeContextTextExM look write except
  where
    look :: VarName
         -> Run SourcePos GWriter Text (GVal (Run SourcePos GWriter Text))
    look k = return $ toGVal $ contextDict !? T.pack k

    write :: Text -> GWriter ()
    write = tellG . Right

    except :: RuntimeError SourcePos -> GWriter ()
    except e@(RuntimeErrorAt p (IndexError i)) = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (show . flip setSourceName (prettyInfo info)) (runtimeErrorWhere e)
      ++ "\nCannot find key " ++ show i ++ " in dictionary\n"
      ++ prettyYNode (toYAML usrDict)
    except e = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (show . flip setSourceName (prettyInfo info)) (runtimeErrorWhere e)
      ++ "\n" ++ T.unpack (runtimeErrorMessage e)
      
    contextDict :: Map (GVal GRunner)
    contextDict = H.insert "placeholder"
                  (fromFunction $ mkPlaceholderFun phFun) $
                  -- H.insert "range"
                  -- (fromFunction rangeF) $
                  -- H.insert "interface"
                  -- (fromFunction returnIfF) $
                  (H.map toGVal usrDict)

-- fromTemplate :: Target l
--              => TplContext
--              -> Source -- -> (Writer Text) Text
--              -> Gen o l Text
-- fromTemplate context tpl = do
--   let options  = mkParserOptions (\_ -> return Nothing)
--       errParse = throwTemplateParse . formatParserError (Just tpl) 
--   template <- either errParse return =<< parseGinger' options tpl
--   let status = execTplM $ runGingerT context (optimize template) 
--   case status of
--     Left err   -> throwTemplateRun err
--     Right code -> return code

-- ------------------------------------------------------------------    
-- -- U kidding me?! Ginger has no 'range' function. I need to define it

-- rangeF :: Monad m => Function (Run p m h)
-- rangeF [] = return def -- TODO: throw error
-- rangeF ((_,x):_) = do
--   xNum <- maybe (throwHere $ ArgumentsError (Just "range")
--                   $ T.pack $ show x ++ " is not a number!")
--           return $ asNumber x
--   xInt <- maybe (throwHere $ ArgumentsError (Just "range")
--                   $ T.pack $ show x ++ " is not an integer!")
--           return $ toBoundedInteger xNum
--   return $ toGVal ([0..xInt-1] :: [Int])

-- returnIfF :: Monad m => Function (Run p m h)
-- returnIfF [] = return def -- TODO: throw error
-- returnIfF ((_,x):_) = do
--   context <- getVar (asText x)
--   return $ toGVal context


-- TODO: yaml2gval helper, passign through JSON. The rest just instantiate ToGVAL
