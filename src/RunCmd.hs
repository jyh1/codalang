{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(cmdExec) where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified System.Process.Typed as P
import qualified RIO.ByteString.Lazy as BL

import Lang.Lang

cmdExec :: Int64 -> Execute -> IO ByteString
cmdExec bufferSize exec = case exec of
    ExecRun env cmdstr opts ->
        procStdout process
        where
            subcmd = "run"
            envstr = buildEnv env
            optArgs = opts
            process = P.proc "cl" (concat [[subcmd], optArgs, envstr, [cmdstr]])
    ExecCat v opts -> do
        let vstr = T.unpack v
            optArgs = opts
        P.runProcess_ (P.setStdout P.createPipe (makeProc ["wait", vstr]))
        (catres, _) <- P.readProcess_ (makeProc ["cat", vstr])
        return (toStrictBytes (BL.take bufferSize catres))
    ExecMake ks opts -> do
        let cmdstr = buildEnv ks
            optArgs = opts
        procStdout (makeProc (concat [["make"], optArgs, cmdstr]))
    where
        procStdout pro = do
            (res, _) <- P.readProcess_ pro
            return (toStrictBytes res)
        makeProc = P.proc "cl"


buildEnv :: Env -> [String]
buildEnv es = envStr <$> es
    where
        envStr (n, e) = T.unpack (T.concat [n, ":", e])
