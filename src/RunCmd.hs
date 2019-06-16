{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(cmdExec) where

import RIO
import qualified RIO.Text as T
import qualified System.Process.Typed as P

import Lang.Lang

cmdExec :: Execute -> IO ByteString
cmdExec exec = case exec of
    ExecRun env cmd opts ->
        procStdout process
        where
            (subcmd, cmdstr) = buildRunCmd cmd
            envstr = buildEnv env
            optArgs = makeOptArgs opts
            process = P.proc "cl" (concat [[subcmd], optArgs, envstr, [cmdstr]])
    ExecCat v -> do
        let vstr = T.unpack v
        P.runProcess_ (P.setStdout P.createPipe (makeProc ["wait", vstr]))
        procStdout (makeProc ["cat", vstr])
    ExecMake ks -> do
        let cmdstr = buildEnv ks
        procStdout (makeProc (concat [["make"], cmdstr]))
    where
        procStdout pro = do
            (res, _) <- P.readProcess_ pro
            return (toStrictBytes res)
        makeProc = P.proc "cl"

buildRunCmd :: [Text] -> (String, String)
buildRunCmd es = ("run", T.unpack (T.intercalate " " es))

buildEnv :: Env -> [String]
buildEnv es = envStr <$> es
    where
        envStr (n, e) = T.unpack (T.concat [n, ":", e])

makeOptArgs :: [ClOption] -> [String]
makeOptArgs = concatMap makeArg
    where
        makeArg (ClName t) = ["-n", T.unpack t]
