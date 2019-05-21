{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RunCmd(runCmd) where

import RIO
import qualified RIO.Text as T
import qualified System.Process.Typed as P

import Lang.Types

runCmd :: Env -> ExecCmd -> IO UUID
runCmd env cmd = do
    (res, _) <- P.readProcess_ process
    let resstr = T.unpack (decodeUtf8Lenient (toStrictBytes res))
    case readMaybe resstr of
        Just uuid -> return (UUID uuid)
        _ -> throwString "No valid UUID returned!"
    where
        (subcmd, cmdstr) = buildCmd cmd
        envstr = buildEnv env
        process = P.proc "cl" (concat [[subcmd], envstr, [cmdstr]])


buildCmd :: ExecCmd -> (String, String)
buildCmd (Run es) = ("run", T.unpack (T.intercalate " " es))

buildEnv :: Env -> [String]
buildEnv es = envStr <$> es
    where
        envStr (n, e) = T.unpack (T.concat [n, ":", e])