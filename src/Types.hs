{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           RIO                     hiding ( view )
import qualified RIO.Text as T
import           RIO.Process
import qualified RIO.Map                       as M
import           Control.Lens

import           Lang.Lang

-- | Command line arguments
data Options = Options
  { 
    optionsVerbose :: !Bool
    , optionsSourceFile :: String
  }

data App = App
  { _appLogFunc :: !LogFunc
  , _appProcessContext :: !ProcessContext
  , _appOptions :: !Options
  , _appClCmd :: Execute -> IO UUID
  }

makeLenses ''App

instance HasLogFunc App where
  logFuncL = appLogFunc
instance HasProcessContext App where
  processContextL = appProcessContext

data LogEnt = EntVerbatim Text | EntUUID Text | EntParen LogEnt
  deriving (Show, Read, Eq)
data LogInfo = Assign [LogEnt] [LogEnt]
  deriving (Show, Read, Eq)

instance Display LogInfo where
  textDisplay (Assign v vs) = T.concat [renderList v, " <- ", renderList vs]
    where
      renderList as = T.intercalate " " (render <$> as)
      render (EntVerbatim t) = t
      render (EntUUID u) = T.take 7 u
      render (EntParen e) = T.concat ["(", render e, ")"]

appLog :: LogInfo -> RIO App ()
appLog l = logInfo (display l)

instance Exec (RIO App) Text where
  clLit vn u = do
    let ustr = tshow u
    appLog (Assign [EntVerbatim vn] [EntUUID ustr])
    return ustr
  clRun vn depMap cmd = do
    clcmd <- view appClCmd
    resid <- tshow <$> liftIO (clcmd execCmd)
    appLog (Assign [EntVerbatim vn, EntParen (EntUUID resid)] (EntVerbatim <$> cmdTxt))
    return resid
   where
    fromEle e = case e of
      Plain t        -> t
      BundleRef r ps -> buildPath (r : ps)
    cmdTxt = fromEle <$> cmd
    fromDep (Deps u subs) = buildPath (u : subs)
    depTxt  = M.toList (fromDep <$> depMap)
    execCmd = ExecRun depTxt cmdTxt [ClName vn]
