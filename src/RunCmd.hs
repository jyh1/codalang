{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(cmdExec) where

import RIO
import qualified RIO.Text as T
import qualified System.Process.Typed as P

import Lang.Lang

cmdExec :: Execute -> IO UUID
cmdExec (ExecRun env cmd opts) = do
    (res, _) <- P.readProcess_ process
    let resstr = toStrictBytes res
    case byteToUUID resstr of
        Just uuid -> return uuid
        Nothing -> throwString "No valid UUID returned!"
    where
        (subcmd, cmdstr) = buildRunCmd cmd
        envstr = buildEnv env
        process = P.proc "cl" (concat [[subcmd], optArgs, envstr, [cmdstr]])
        optArgs = makeOptArgs opts


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
