{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Types where

import           RIO
import           Numeric                        ( showHex )
import           Control.Lens                   ( makeLenses
                                                )

-- UUID of CodaLab bundle
newtype UUID = UUID {unuuid :: Integer}
    deriving (Eq, Ord, Read)

instance Show UUID where
    show (UUID n) = "0x" ++ zeros ++ showHex n ""
        where
            shown = showHex n ""
            -- zeros = replicate (32 - length shown) '0'
            zeros = ""

type VarName = Text

-- Codalab command

newtype Cmd a = Run {_runcmd :: [a]}
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
    | Str Text
    | Dir CodaVal Text
    | Let VarName CodaVal CodaVal
    deriving (Eq, Ord, Read, Show)

tmpName :: Text
tmpName = "codalang"

data CodaType = TypeString | TypeBundle
    deriving (Eq, Ord, Read, Show)