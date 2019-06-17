{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(cmdExec) where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified System.Process.Typed as P

import Lang.Lang

cmdExec :: Execute -> IO ByteString
cmdExec exec = case exec of
    ExecRun env cmd opts ->
        procStdout process
        where
            (subcmd, cmdstr) = buildRunCmd cmd
            envstr = buildEnv env
            optArgs = makeOptArgs runOptions opts
            process = P.proc "cl" (concat [[subcmd], optArgs, envstr, [cmdstr]])
    ExecCat v opts -> do
        let vstr = T.unpack v
            optArgs = makeOptArgs catOptions opts
        P.runProcess_ (P.setStdout P.createPipe (makeProc ["wait", vstr]))
        procStdout (makeProc ["cat", vstr])
    ExecMake ks opts -> do
        let cmdstr = buildEnv ks
            optArgs = makeOptArgs makeOptions opts
        procStdout (makeProc (concat [["make"], optArgs, cmdstr]))
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

makeOptArgs :: S.Set Text -> ClOption -> [String]
makeOptArgs optnames opts = concatMap makeArg (M.toList avaiableOpts)
    where
        avaiableOpts = M.restrictKeys (M.fromList opts) optnames
        makeArg (name, val) = [T.unpack ("--" <> name), T.unpack val]

runOptions, makeOptions, catOptions :: S.Set Text
runOptions = S.fromList [
    "name"
    , "description"
    , "tags"
    , "allow-failed-dependencies"
    , "request-docker-image"
    , "request-time"
    , "request-memory"
    , "request-disk"
    , "request-cpus"
    , "request-gpus"
    , "request-queue"
    , "request-priority"
    , "request-network"
    ]
makeOptions = S.fromList ["name"]
catOptions = S.fromList []