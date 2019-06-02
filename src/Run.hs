{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import RIO
import qualified RIO.Text as T

import Types
import Lang.Lang

run :: RIO App ()
run = do
  source <- view (appOptions . to optionsSourceFile)
  logInfo (display ("parsing from " <> T.pack source))
  parsed <- loadFile source
  codaAST <- maybe (throwString "") return parsed
  logInfo "type checking"
  runTypeCheck codaAST
  logInfo "remove-complex-operation"
  let rcoAST = runRCO codaAST
  res <- evalCoda rcoAST :: RIO App (RuntimeRes Text)
  logInfo (display (tshow res))

  -- logInfo "Codalang"
runTypeCheck :: (MonadIO m) => CodaVal -> m ()
runTypeCheck cv = case (typeCheck cv) of
  Left err -> throwString (T.unpack err) >> return ()
  Right _ -> return ()