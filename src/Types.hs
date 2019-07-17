{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           RIO                     hiding ( view, over, (^.), to, Lens' )
import qualified RIO.Text as T
import Control.Monad.State
import qualified RIO.Text.Partial as T
import qualified RIO.ByteString as B
import RIO.List.Partial
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (showLitChar)
import           RIO.Process
import qualified RIO.Map                       as M
import           Control.Lens
import qualified Network.Wreq as Wreq

import           Lang.Lang

-- | Command line arguments
data Options = Options
  { 
    optionsVerbose :: !Bool
    , optionsExpr :: String
    , optionsBufferSize :: Int64
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

data LogEnt = EntVerbatim Text | EntUUID Text | EntParen LogEnt | EntOptEnv (ClInfo Text)
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
      render (EntOptEnv e) = render (EntVerbatim (_codaName e))

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
    resid <- parseUUID execRes
    appLog (Assign [EntOptEnv vn, EntParen (EntUUID resid)] (escapeVerbatim <$> cmdTxt))
    return resid
   where
    cmdTxt = fromEle <$> cmd
    depTxt  = M.toList depMap
    execCmd = ExecRun depTxt cmdTxt (consOptionList vn)
  clCat vn val = do
    clcmd <- view appClCmd
    res <- liftIO (clcmd execCmd)
    let resText = decodeUtf8Lenient res
    appLog (Assign [EntOptEnv vn] [EntVerbatim (tshow resText)])
    return resText
    where
      execCmd = ExecCat val (consOptionList vn)
  clMake vn ks = do
    clcmd <- view appClCmd
    let makeCmd = ExecMake ks (consOptionList vn)
    execRes <- liftIO (clcmd makeCmd) >>= parseUUID
    appLog (Assign [EntOptEnv vn] [EntUUID execRes])
    return execRes
  strLit s = return s
  fromBundleName bn = return bn
  execDir d p = return (buildPath [d, p])
  execRec mp = return (tshow mp)

type LoadState = [Module]
loadStack :: Lens' LoadState [Module]
loadStack = id
stackInf :: LoadState -> Text
stackInf ls = tshow (view (loadStack . to (map moduleTxt)) ls)

instance LoadModule (StateT LoadState (RIO App)) where
  loadModule m app = do
    hasCircle <- use (loadStack . to listMem)
    bool doLoad circleErr hasCircle
    where
      listMem = anyOf traverse (== m)
      circleErr = do
        loadTxt <- use (to stackInf)
        logError ("Circle depedency during parsing: " <> display loadTxt)
        throwString ("parsing error")
      doLoad = do
        loadStack %= (m :)
        content <- case m of
          SysPath p -> B.readFile (T.unpack p)
          CodaBundle b -> do
            clcmd <- view appClCmd
            liftIO (clcmd (ExecCat b []))
          URL l -> do
            r <- liftIO $ Wreq.get (T.unpack l)
            buffsize <- view (appOptions . to optionsBufferSize)
            return (BL.toStrict (BL.take buffsize (r ^. Wreq.responseBody)))
        res <- app content
        loadStack %= tail
        return res
  parseError e = do
    loadTxt <- use (to stackInf)
    logError ("During parsing: " <> display loadTxt)
    logError (display (T.pack e))
    throwString ("parsing error")

runParser :: String -> RIO App CodaVal
runParser s = evalStateT parse []
    where
      parse :: StateT [Module] (RIO App) CodaVal
      parse = loadString s

consOptionList :: (ClInfo Text) -> ClOption
consOptionList (ClInfo vname optList) = ("name", vname) : optList

-- fromDeps :: Deps Text -> Text
-- fromDeps (Deps r ps) = buildPath (r : ps)

fromEle :: CMDEle Text Text -> Text
fromEle e = case e of
    Plain t        -> t
    BundleRef r ps -> buildPath (r:ps)
      
parseUUID :: ByteString -> RIO App Text
parseUUID res = do
  let uuid = maybe (Left (stringException "No valid UUID returned!")) Right (byteToUUID res)
  tshow <$> fromEither uuid