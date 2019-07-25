{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- codalang compiled json compilation
module Lang.JLang where

import Lang.Types
import Lang.Interpret
import Types

import RIO
import Control.Monad.Writer
import qualified RIO.Seq as S
import qualified RIO.Map as M
import qualified RIO.Text as T
import qualified RIO.HashMap as HM
import Data.Aeson


type JCmdOpt = [(Text, JRes)]
data JBlock a = JBlock !Text !JCmdOpt !(JCmd a)
    deriving (Show, Read, Eq)
data JCmd a = JCat a | JMake [(Text, a)] | JRun [(Text, a)] [CMDEle Text a] | JLit Text
    deriving (Show, Read, Eq)
data JRes = JVariable Text | JVerbatim Text | JDir JRes [Text] | JRec (TextMap JRes)
    deriving (Show, Read, Eq, Ord)
data JLang = JLang JRes [JBlock JRes]
    deriving (Show, Read, Eq)

type JBlocks = S.Seq (JBlock JRes)

type JCompileEnv = Writer JBlocks

makeKV :: (KeyValue kv) => JRes -> [kv]
makeKV x = case x of
    JVariable t -> [tagType "variable", tagContent t]
    JVerbatim t -> [tagType "value", tagContent t]
    JDir b ps -> [tagType "dir", tagContent (object ["root" .= b, "path" .= ps])]
    JRec tm -> [tagType "record", tagContent (object ((uncurry (.=)) <$> (M.toList tm)))]

instance ToJSON JRes where
    toJSON x = object (makeKV x)
    toEncoding x = doEncode (makeKV x)
instance FromJSON JRes where
    parseJSON (Object v) = do
        ty :: Text <- v .: "type"
        cont :: Value <- v .: "content"
        case ty of
            "variable" -> JVariable <$> parseJSON cont
            "value" -> JVerbatim <$> parseJSON cont
            "dir" -> do
                obj :: Object <- parseJSON cont
                JDir <$> obj .: "root" <*> obj .: "path"
            "record" -> do
                obj :: Object <- parseJSON cont
                objres <- mapM parseJSON obj
                return (JRec (M.fromList (HM.toList objres)))
            _ -> fail ("Unkonw type in JRes: " ++ T.unpack ty)
    parseJSON invalid = fail ("Error parsing JRes: " ++ show invalid)

makeCmdKV :: (KeyValue kv, ToJSON a) => JCmd a -> [kv]
makeCmdKV x = case x of
    JCat a -> [tagType "cat", tagContent a]
    JMake ks -> [tagType "make", tagContent ks]
    JRun deps cmd -> [tagType "run", tagContent content]
        where
            content = object ["dependencies" .= deps, "cmd" .= cmd]
    JLit a -> [tagType "lit", tagContent a]
instance (ToJSON a) => ToJSON (JCmd a) where
    toJSON x = object (makeCmdKV x)
    toEncoding x = doEncode (makeCmdKV x)
instance (FromJSON a) => FromJSON (JCmd a) where
    parseJSON (Object v) = do
        ty :: Text <- v .: "type"
        cont :: Value <- v .: "content"
        case ty of
            "cat" -> JCat <$> parseJSON cont
            "make" -> JMake <$> parseJSON cont
            "run" -> do
                obj :: Object <- parseJSON cont
                JRun <$> obj .: "dependencies" <*> obj .: "cmd"
            "lit" -> JLit <$> parseJSON cont
            _ -> fail ("Unknow types in JCmd: " ++ T.unpack ty)
    parseJSON invalid = fail ("Error parsing JCmd: " ++ show invalid)

makeBlkCV :: (ToJSON a, KeyValue kv) => JBlock a -> [kv]
makeBlkCV (JBlock v opt cmd) = ["variable" .= v, "options" .= opt, "command" .= cmd]
instance (ToJSON a) => ToJSON (JBlock a) where
    toJSON x = object (makeBlkCV x)
    toEncoding x = doEncode (makeBlkCV x)
instance (FromJSON a) => FromJSON (JBlock a) where
    parseJSON (Object v) = JBlock <$> v .: "variable" 
        <*> v .: "options" <*> v .: "command"
    parseJSON invalid = fail ("Error parsing JBlock: " ++ show invalid)

makeJlCV :: (KeyValue kv) => JLang -> [kv]
makeJlCV (JLang res blks) = ["result" .= res, "blocks" .= blks]    
instance ToJSON JLang where
    toJSON x = object (makeJlCV x)
    toEncoding x = doEncode (makeJlCV x)
instance FromJSON JLang where
    parseJSON (Object v) = JLang <$> v .: "result" <*> v .: "blocks"
    parseJSON invalid = fail ("Error parsing JLang: " ++ show invalid)

instance Exec JCompileEnv JRes where
    clLit vn u = do
        let blk = JBlock vn [] (JLit (tshow u))
        writeBlk blk
        return (JVariable vn)
    clCat clinfo res = makeJBlk clinfo (JCat res)
    clRun clinfo tm eles = makeJBlk clinfo (JRun (M.toList tm) eles)
    clMake clinfo ks = makeJBlk clinfo (JMake ks)
    strLit s = return (JVerbatim s)
    fromBundleName b = return b
    execDir d path = return $ case d of
        JDir b ps -> JDir b (ps ++ [path])
        _ -> JDir d [path]
    execRec m = return (JRec m)


makeJBlk :: ClInfo JRes -> JCmd JRes -> JCompileEnv JRes
makeJBlk (ClInfo vname optList) cmd = do
    let blk = JBlock vname optList cmd
    writeBlk blk
    return (JVariable vname)

writeBlk :: JBlock JRes -> JCompileEnv ()
writeBlk blk = tell (S.singleton blk)

compileJ :: CodaVal -> (JRes, [JBlock JRes])
compileJ cv = toList <$> runIdentity (runWriterT app)
    where
        app :: JCompileEnv JRes
        app = evalCoda cv

toJLang :: CodaVal -> JLang
toJLang c = uncurry JLang (compileJ c)