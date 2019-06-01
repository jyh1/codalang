{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LangTest.Interpret(testInterpret) where

-- a dummy interpreter for CodaVal for testing purpose

import RIO hiding (to)
import Control.Lens
import qualified RIO.Text as T
import Control.Monad.State

import Lang.Types
import Lang.Fold

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