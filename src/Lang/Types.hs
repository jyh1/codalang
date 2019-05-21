{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Types where

import           RIO
import           Numeric                        ( showHex )
import           Control.Lens                   ( makeLenses
                                                , Prism
                                                , prism
                                                )

-- UUID of CodaLab bundle
newtype UUID = UUID Integer
    deriving (Eq, Ord, Read)

instance Show UUID where
    show (UUID n) = "0x" ++ zeros ++ showHex n ""
        where
            shown = showHex n ""
            zeros = replicate (32 - length shown) '0'

type VarName = Text

-- Codalab command
data CmdEle a = Verbatim Text | Val a
    deriving (Eq, Ord, Read, Show, Functor)

cmdEleVal :: Prism (CmdEle a) (CmdEle b) a b
cmdEleVal = prism Val getVal
  where
    getVal (Val      a) = Right a
    getVal (Verbatim t) = Left (Verbatim t)

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

tmpName :: Text
tmpName = "codalang"
