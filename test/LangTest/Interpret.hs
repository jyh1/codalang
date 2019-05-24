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

data CodaTestRes = BunRes Integer | StrRes Text | RunRes [CodaTestRes] | DirRes CodaTestRes [Text]
    deriving (Show, Read, Eq, Ord)

data CodaInterEnv a = CodaInterEnv {_envmap :: VarMap a, _cmdlog :: [[a]]}
    deriving (Show, Read, Eq)
makeLenses ''CodaInterEnv

type InterApp = StateT (CodaInterEnv CodaTestRes) Identity

instance HasEnv (CodaInterEnv a) a where
    envL = envmap

instance LocalVar (CodaInterEnv CodaTestRes) InterApp CodaTestRes

instance CodaLangEnv InterApp CodaTestRes where
    lit = return . BunRes . unuuid
    str = return . StrRes
    var v = use (envL . at v . to (fromMaybe errmsg))
        where
            errmsg = error ("Undefined var in test interpreter: " ++ T.unpack v)
    cl (Run cmd) = do
        -- logging run command
        cmd' <- sequence cmd
        cmdlog %= (cmd' :)
        return (RunRes cmd')
    dir val sub = return $ case val of
        DirRes v subs -> DirRes v (subs ++ [sub])
        other -> DirRes other [sub]
    clet varn val body = do
        valres <- val
        withVar varn valres body

-- return logs of runned command and final result
testInterpret :: CodaVal -> ([[CodaTestRes]], CodaTestRes)
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty []))