{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- codalang compiled json compilation
module Lang.JLang where

import Lang.Types
import Lang.Interpret
import Types

import RIO
import Control.Monad.Writer
import qualified RIO.Seq as S
import qualified RIO.Map as M


type JCmdOpt = [(Text, JRes)]
data JBlock a = JBlock !Text !JCmdOpt !(JCmd a)
    deriving (Show, Read, Eq)
data JCmd a = JCat a | JMake [(Text, a)] | JRun [(Text, a)] [CMDEle Text a] | JLit a
    deriving (Show, Read, Eq)
data JRes = JVariable Text | JVerbatim Text | JDir JRes [Text] | JRec (TextMap JRes)
    deriving (Show, Read, Eq, Ord)

type JBlocks = S.Seq (JBlock JRes)

type JCompileEnv = Writer JBlocks

instance Exec JCompileEnv JRes where
    clLit vn u = do
        let blk = JBlock vn [] (JLit (JVerbatim (tshow u)))
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