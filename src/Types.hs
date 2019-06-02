{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           RIO                     hiding ( view )
import           RIO.Process
import qualified RIO.Map                       as M
import           Control.Lens

import           Lang.Lang

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { _appLogFunc :: !LogFunc
  , _appProcessContext :: !ProcessContext
  , _appOptions :: !Options
  , _appClCmd :: Execute -> IO UUID
  , _appSourceFile :: String
  }

makeLenses ''App

instance HasLogFunc App where
  logFuncL = appLogFunc
instance HasProcessContext App where
  processContextL = appProcessContext


instance Exec (RIO App) Text where
  clLit _ u = return (tshow u)
  clRun _ depMap cmd = do
    clcmd <- view appClCmd
    tshow <$> liftIO (clcmd execCmd)
   where
    fromEle e = case e of
      Plain t        -> t
      BundleRef r ps -> buildPath (r : ps)
    cmdTxt = fromEle <$> cmd
    fromDep (Deps u subs) = buildPath (u : subs)
    depTxt  = M.toList (fromDep <$> depMap)
    execCmd = ExecRun depTxt cmdTxt
