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
import qualified RIO.Map as M
import           Control.Lens

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
    -- value targettype fromtype
    | Convert (Maybe CodaType) CodaVal CodaType
    | Dict (Map Text CodaVal)
    deriving (Eq, Ord, Read, Show)

defConvert :: CodaVal -> CodaType -> CodaVal
defConvert = Convert Nothing

tmpName :: Text
tmpName = "codalang"

type TextMap = Map Text
type TypeDict = TextMap CodaType
data CodaType = TypeString | TypeBundle | TypeRecord TypeDict
    deriving (Eq, Ord, Read, Show)

isSubtypeOf :: CodaType -> CodaType -> Bool
isSubtypeOf (TypeRecord d1) (TypeRecord d2) = dictMinus isSubtypeOf d2 d1
isSubtypeOf TypeBundle (TypeRecord d1) = allOf traverse (TypeBundle `isSubtypeOf`) d1
isSubtypeOf t1 t2 = t1 == t2

convertable :: CodaType -> CodaType -> Bool
convertable t1 t2
    | t1 `isSubtypeOf` t2 = True
    | otherwise = case (t1, t2) of
        (TypeRecord{}, TypeString) -> False
        (TypeString, TypeRecord{}) -> False
        (TypeRecord d1, TypeRecord d2) -> dictMinus convertable d2 d1
        _ -> True

dictMinus :: (a -> a -> Bool) -> TextMap a -> TextMap a -> Bool
dictMinus f d1 d2 = M.null (M.differenceWith maybediff d1 d2)
    where
        maybediff t2 t1 = bool (Just t2) Nothing (f t1 t2)

data CodaResult = ResStr Text | ResBundle UUID
    deriving (Eq, Ord, Read, Show)

data ClOption = ClName Text
    deriving (Show, Read, Eq)

data Execute = ExecRun [(Text, Text)] [Text] [ClOption]
    | ExecCat Text
    deriving (Show, Read, Eq)

buildPath :: [Text] -> Text
buildPath = T.intercalate "/"

data Deps a = Deps a [Text]
    deriving (Show, Read, Eq, Ord)

data CMDEle a = Plain Text | BundleRef a [Text]
    deriving (Show, Read, Eq, Functor, Ord)

class (Monad m) => Exec m a where
    clRun :: Text -> Map Text (Deps a) -> [CMDEle Text] -> m a
    clCat :: Text -> Deps a -> m (RuntimeRes a)
    clLit :: Text -> UUID -> m a

data RuntimeRes a = RuntimeString Text | RuntimeBundle a [Text]
    deriving (Show, Read, Eq, Ord)
