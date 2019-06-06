{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Lang.Types where

import           RIO
import qualified RIO.Text as T
import           Control.Lens                   ( makeLenses
                                                )

-- UUID of CodaLab bundle
data UUID = UUID Text | BundleName Text
    deriving (Eq, Ord, Read)

instance Show UUID where
    show (UUID n) = "0x" ++ T.unpack n
    show (BundleName t) = T.unpack t

type VarName = Text

-- Codalab command

data Cmd a = Run [a] | ClCat a
    deriving (Eq, Ord, Read, Show, Functor)
instance Foldable Cmd where
    foldMap f (ClCat a) = f a
    foldMap f (Run as) = foldMap f as
instance Traversable Cmd where
    traverse f (ClCat a) = ClCat <$> (f a)
    traverse f (Run as) = Run <$> (traverse f as)

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
    | Convert CodaVal CodaType
    deriving (Eq, Ord, Read, Show)

tmpName :: Text
tmpName = "codalang"

data CodaType = TypeString | BundleDic (Map Text CodaType)
    deriving (Eq, Ord, Read, Show)
typeBundle :: CodaType
typeBundle = BundleDic mempty

data CodaResult = ResStr Text | ResBundle UUID
    deriving (Eq, Ord, Read, Show)

data ClOption = ClName Text
    deriving (Show, Read, Eq)

data Execute = ExecRun [(Text, Text)] [Text] [ClOption]
    deriving (Show, Read, Eq)

buildPath :: [Text] -> Text
buildPath = T.intercalate "/"

data Deps a = Deps a [Text]
    deriving (Show, Read, Eq, Ord)

data CMDEle a = Plain Text | BundleRef a [Text]
    deriving (Show, Read, Eq, Functor, Ord)

class (Monad m) => Exec m a where
    clRun :: Text -> Map Text (Deps a) -> [CMDEle Text] -> m a
    clLit :: Text -> UUID -> m a

data RuntimeRes a = RuntimeString Text | RuntimeBundle a [Text]
    deriving (Show, Read, Eq, Ord)
