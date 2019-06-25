{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Types
import RIO
import Run
import RunCmd
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_codalang

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_codalang.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
      
      <*> inputExpr
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = options
          , _appClCmd = cmdExec
          }
     in runRIO app run


inputExpr :: Parser String
inputExpr = codaExpr <|> inputFile
  where
    codaExpr = strOption (
      short 'c'
      <> metavar "EXPR"
      <> help "input expression"
      )
    inputFile = ("%load " ++ ) <$> strArgument (metavar "ARGUMENT" <> help msg)
      where
        msg = "this is equivalent with '%load ARUGMENT', system filepath should always start with '/' or './'"