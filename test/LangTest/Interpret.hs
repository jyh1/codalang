{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LangTest.Interpret(testInterpret, testInterpretWIntrfc) where

-- a dummy interpreter for CodaVal for testing purpose

import RIO hiding (to, view, traceShow)
import Control.Lens
import qualified RIO.Text as T
import Control.Monad.State
import RIO.List.Partial (head, tail)
import qualified RIO.Map as M
import Debug.Trace (traceShow)

import Lang.Types
import Lang.Fold
import Lang.Interpret

data CodaTestRes = BunRes UUID | RunRes Int | DirRes CodaTestRes [Text] | StrRes Text | VarRes Text
    deriving (Show, Read, Eq, Ord)

data CmdLog a = LogRun [a]
    deriving (Show, Read, Eq, Ord)

data CodaInterEnv a = CodaInterEnv {_envmap :: VarMap a, _cmdlog :: [CmdLog a], _counter :: Int}
    deriving (Show, Read, Eq)
makeLenses ''CodaInterEnv

type InterApp = StateT (CodaInterEnv CodaTestRes) Identity

instance HasEnv (CodaInterEnv a) a where
    envL = envmap

instance HasCounter (CodaInterEnv a) where
    counterL = counter
instance GetCounter (CodaInterEnv CodaTestRes) InterApp

instance LocalVar (CodaInterEnv CodaTestRes) InterApp CodaTestRes

instance CodaLangEnv InterApp CodaTestRes where
    lit = return . BunRes
    str = return . StrRes
    var v = use (envL . at v . to (fromMaybe errmsg))
        where
            errmsg = error ("Undefined var in test interpreter: " ++ T.unpack v)
    cl (Run cmd) = do
        -- logging run command
        cmd' <- sequence cmd
        cmdlog %= (LogRun cmd' :)
        runid <- getCounter
        return (RunRes runid)
    dir val sub = return $ case val of
        DirRes v subs -> DirRes v (subs ++ [sub])
        other -> DirRes other [sub]
    clet varn val body = do
        valres <- val
        withVar varn valres body

-- return logs of runned command and final result
testInterpret :: CodaVal -> ([CmdLog CodaTestRes], CodaTestRes)
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0))

-- use interpret interface (after RCO)
instance Exec InterApp CodaTestRes where
    clRun _ deps cmd = do
        cmdlog %= (LogRun resCmd :)
        c <- getCounter
        return (RunRes c)
        where
            resCmd = parseEle <$> cmd
            parseEle ele
                -- | traceShow eleDep False = undefined 
                -- | length paths == 1 = StrRes ele
                | otherwise = case ele of
                    Plain t -> StrRes t
                    BundleRef eleVar ps -> 
                        let eleDep = view (at eleVar . to (fromMaybe undefined)) deps in 
                            fromDep eleDep ps
                            
            fromDep (Deps tres depPath) elePath
                | null ps = tres
                | otherwise = case tres of
                    DirRes r sub -> DirRes r (sub ++ ps)
                    _ -> DirRes tres ps
                where
                    ps = depPath ++ elePath
    clLit _ u = return (BunRes u)

testInterpretWIntrfc :: CodaVal -> ([CmdLog CodaTestRes], CodaTestRes)
testInterpretWIntrfc cv = (_cmdlog env, newRes)
    where
        app :: InterApp (RuntimeRes CodaTestRes)
        app = evalCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0))
        newRes = case res of
            RuntimeString t -> StrRes t
            RuntimeBundle m ps -> case (m, ps) of
                (DirRes r rps, _) -> DirRes r (rps ++ ps)
                (other, []) -> other
                (other, ps) -> DirRes other ps