{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Types where

import RIO
import Numeric (showHex)
import Control.Lens (makeLenses)

-- UUID of CodaLab bundle
newtype UUID = UUID Integer
    deriving (Eq, Ord, Read)

instance Show UUID where
    show (UUID n) = "0x" ++ showHex n ""

type VarName = Text

-- Codalab command
data CmdEle a = Verbatim Text | Val a
    deriving (Eq, Ord, Read, Show, Functor)


newtype Cmd a = Bash {_bashcmd :: [CmdEle a]}
    deriving (Eq, Ord, Read, Show, Functor)

makeLenses ''Cmd

type CodaCmd = Cmd CodaVal
type ExecCmd = Cmd Text

-- Command options
type Env = [(Text, Text)]

-- CodaLang AST
data CodaVal = Lit UUID 
    | Var VarName 
    | Cl CodaCmd 
    | Dir CodaVal Text
    | Let VarName CodaVal CodaVal
    deriving (Eq, Ord, Read, Show)