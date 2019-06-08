{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           RIO                     hiding ( view )
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import RIO.Char (showLitChar)
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
  , _appClCmd :: Execute -> IO ByteString
  }

makeLenses ''App

instance HasLogFunc App where
  logFuncL = appLogFunc
instance HasProcessContext App where
  processContextL = appProcessContext

data LogEnt = EntVerbatim Text | EntUUID Text | EntParen LogEnt
  deriving (Show, Read, Eq)
escapeVerbatim :: Text -> LogEnt
escapeVerbatim t = EntVerbatim (T.pack (T.foldr showLitChar "" t))
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
    execRes <- liftIO (clcmd execCmd)
    let uuid = maybe (Left (stringException "No valid UUID returned!")) Right (byteToUUID execRes)
    resid <- tshow <$> fromEither uuid
    appLog (Assign [EntVerbatim vn, EntParen (EntUUID resid)] (escapeVerbatim <$> cmdTxt))
    return resid
   where
    cmdTxt = map fromEle cmd
    fromDep (Deps u subs) = buildPath (u : subs)
    depTxt  = M.toList (fromDep <$> depMap)
    execCmd = ExecRun depTxt cmdTxt [ClName vn]
  clCat vn val = do
    clcmd <- view appClCmd
    res <- liftIO (clcmd execCmd)
    let resText = decodeUtf8Lenient res
    appLog (Assign [EntVerbatim vn] [EntVerbatim (tshow resText)])
    return (RuntimeString resText)
    where
      execCmd = ExecCat (fromDeps val)

fromDeps :: Deps Text -> Text
fromDeps (Deps r ps) = buildPath (r : ps)

fromEle :: CMDEle Text -> Text
fromEle e = case e of
    Plain t        -> t
    BundleRef r ps -> fromDeps (Deps r ps)
      