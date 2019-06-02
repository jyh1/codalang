{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Types where

import           RIO
import qualified RIO.Text as T
import           Numeric                        ( showHex )
import           Control.Lens                   ( makeLenses
                                                )

-- UUID of CodaLab bundle
newtype UUID = UUID {unuuid :: Integer}
    deriving (Eq, Ord, Read)

instance Show UUID where
    show (UUID n) = "0x" ++ zeros ++ showHex n ""
        where
            -- shown = showHex n ""
            -- zeros = replicate (32 - length shown) '0'
            zeros = ""

type VarName = Text

-- Codalab command

newtype Cmd a = Run {_runcmd :: [a]}
    deriving (Eq, Ord, Read, Show, Functor)

makeLenses ''Cmd

type CodaCmd = Cmd CodaVal

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
    deriving (Eq, Ord, Read)
instance Show CodaType where
    show TypeString = "String"
    show TypeBundle = "{_}"

data CodaResult = ResStr Text | ResBundle UUID
    deriving (Eq, Ord, Read, Show)

data Execute = ExecRun [(Text, Text)] [Text]
    deriving (Show, Read, Eq)

buildPath :: [Text] -> Text
buildPath = T.intercalate "/"

data Deps a = Deps a [Text]
    deriving (Show, Read, Eq)

data CMDEle = Plain Text | BundleRef Text [Text]
    deriving (Show, Read, Eq)

class (Monad m) => Exec m a where
    clRun :: Text -> Map Text (Deps a) -> [CMDEle] -> m a
    clLit :: Text -> UUID -> m a

data RuntimeRes a = RuntimeString Text | RuntimeBundle a [Text]
    deriving (Show, Read, Eq)