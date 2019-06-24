{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import RIO
import qualified RIO.Text as T
import Control.Lens (bimap)

import Types
import Lang.Lang

run :: RIO App ()
run = do
  source <- view (appOptions . to optionsSourceFile)
  logInfo (display ("parsing from " <> T.pack source))
  codaAST <- loadFile source
  logInfo "type checking"
  tcAST <- runTypeCheck codaAST
  logInfo "remove-complex-operation"
  let rcoAST = runRCO tcAST
  res <- evalCoda rcoAST :: RIO App (RuntimeRes Text)
  logInfo (display (tshow res))

  -- logInfo "Codalang"
runTypeCheck :: (MonadIO m) => CodaVal -> m CodaVal
runTypeCheck cv = fromEither (bimap (stringException . T.unpack) snd (typeCheck cv))