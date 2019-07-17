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
  source <- view (appOptions . to optionsExpr)
  logInfo (display ("executing " <> T.pack source))
  codaAST <- runParser source
  logInfo "type checking"
  (tcTy, tcAST) <- runTypeCheck codaAST
  if containLambda tcTy then
    logInfo (display $ pprintType tcTy)
  else do
    logInfo "remove-complex-operation"
    let rcoAST = runRCO tcAST
    res <- evalCoda rcoAST :: RIO App Text
    logInfo (display res)

  -- logInfo "Codalang"
runTypeCheck :: (MonadIO m) => CodaVal -> m (CodaType, CodaVal)
runTypeCheck cv = fromEither (bimap (stringException . T.unpack) id (typeCheck cv))
