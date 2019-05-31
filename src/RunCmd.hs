{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(cmdExec) where

import RIO
import qualified RIO.Text as T
import qualified System.Process.Typed as P

import Lang.Types

cmdExec :: Execute -> IO UUID
cmdExec (ExecRun env cmd) = do
    (res, _) <- P.readProcess_ process
    let resstr = T.unpack (decodeUtf8Lenient (toStrictBytes res))
    case readMaybe resstr of
        Just uuid -> return (UUID uuid)
        _ -> throwString "No valid UUID returned!"
    where
        (subcmd, cmdstr) = buildRunCmd cmd
        envstr = buildEnv env
        process = P.proc "cl" (concat [[subcmd], envstr, [cmdstr]])


buildRunCmd :: [Text] -> (String, String)
buildRunCmd es = ("run", T.unpack (T.intercalate " " es))

buildEnv :: Env -> [String]
buildEnv es = envStr <$> es
    where
        envStr (n, e) = T.unpack (T.concat [n, ":", e])