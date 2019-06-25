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

data Cmd a = Run [a] | ClCat a | ClMake [(Text, a)]
    deriving (Eq, Ord, Read, Show, Functor)
instance Foldable Cmd where
    foldMap f (ClCat a) = f a
    foldMap f (Run as) = foldMap f as
    foldMap f (ClMake rs) = foldMap (foldMap f) rs
instance Traversable Cmd where
    traverse f (ClCat a) = ClCat <$> (f a)
    traverse f (Run as) = Run <$> (traverse f as)
    traverse f (ClMake rs) = ClMake <$> ((traverse . _2) f rs)

type CodaCmd = Cmd CodaVal

-- Command options
type Env = [(Text, Text)]

-- assign to let
data AssignForm = Variable VarName
    | OptionVar VarName
    deriving (Eq, Ord, Read, Show)

printAssignForm :: AssignForm -> Text
printAssignForm af = case af of
    Variable v -> v
    OptionVar v -> "--" <> v

type OptEnv = [(Text, CodaVal)]
-- CodaLang AST
data CodaVal = Lit UUID
    | Var VarName
    | Cl OptEnv CodaCmd
    | Str Text
    | Dir CodaVal Text
    | Let AssignForm CodaVal CodaVal
    -- value targettype fromtype
    | Convert (Maybe CodaType) CodaVal CodaType
    | Dict (TextMap CodaVal)
    | Lambda TypeDict CodaVal
    | Apply CodaVal (TextMap CodaVal)
    deriving (Eq, Ord, Read, Show)

makeCl :: CodaCmd -> CodaVal
makeCl = Cl []

defConvert :: CodaVal -> CodaType -> CodaVal
defConvert = Convert Nothing

tmpName :: Text
tmpName = "codalang"

type TextMap = Map Text
type TypeDict = TextMap CodaType
data CodaType = TypeString | TypeBundle | TypeRecord TypeDict | TypeLam TypeDict CodaType
    deriving (Eq, Ord, Read, Show)

containLambda :: CodaType -> Bool
containLambda ty = case ty of
    TypeLam{} -> True
    TypeRecord tr -> allOf traverse containLambda tr
    _ -> False

isSubtypeOf :: CodaType -> CodaType -> Bool
isSubtypeOf (TypeRecord d1) (TypeRecord d2) = dictMinus isSubtypeOf d2 d1
isSubtypeOf TypeBundle (TypeRecord d1) = allOf traverse (TypeBundle `isSubtypeOf`) d1
isSubtypeOf (TypeLam arg1 body1) (TypeLam arg2 body2) = 
    (TypeRecord arg2) `isSubtypeOf` (TypeRecord arg1) && body1 `isSubtypeOf` body2
isSubtypeOf t1 t2 = t1 == t2

convertable :: CodaType -> CodaType -> Bool
convertable t1 t2
    | t1 `isSubtypeOf` t2 = True
    | otherwise = case (t1, t2) of
        (TypeRecord{}, TypeString) -> False
        (TypeString, TypeRecord{}) -> False
        (TypeLam{}, _) -> False
        (_, TypeLam{}) -> False
        (TypeRecord d1, TypeRecord d2) -> dictMinus convertable d2 d1
        (TypeBundle, TypeRecord d) -> allOf traverse (TypeBundle `convertable`) d
        (TypeRecord d, TypeBundle) -> allOf traverse (`convertable` TypeBundle) d
        _ -> True

dictMinus :: (a -> a -> Bool) -> TextMap a -> TextMap a -> Bool
dictMinus f d1 d2 = M.null (M.differenceWith maybediff d1 d2)
    where
        maybediff t2 t1 = bool (Just t2) Nothing (f t1 t2)

data CodaResult = ResStr Text | ResBundle UUID
    deriving (Eq, Ord, Read, Show)

type ClOption = [(Text, Text)]

data Execute = ExecRun [(Text, Text)] [Text] ClOption
    | ExecCat Text ClOption
    | ExecMake [(Text, Text)] ClOption
    deriving (Show, Read, Eq)

buildPath :: [Text] -> Text
buildPath = T.intercalate "/"

data Deps a = Deps a [Text]
    deriving (Show, Read, Eq, Ord)

data CMDEle a = Plain Text | BundleRef a [Text]
    deriving (Show, Read, Eq, Functor, Ord)


class (Monad m) => Exec m a where
    clRun :: (ClInfo a) -> Map Text (Deps a) -> [CMDEle Text] -> m a
    clCat :: (ClInfo a) -> Deps a -> m (RuntimeRes a)
    clLit :: Text -> UUID -> m a
    clMake :: (ClInfo a) -> [(Text, Deps a)] -> m a

data RuntimeRes a = RuntimeString Text 
    | RuntimeBundle a [Text] 
    | RuntimeRecord [(Text, RuntimeRes a)]
    deriving (Show, Read, Eq, Ord)

data ClInfo a = ClInfo {_codaName :: Text, _clOpt :: [(Text, RuntimeRes a)]}
    deriving (Show, Read, Eq)
makeLenses ''ClInfo


data Module = URL Text | SysPath Text | CodaBundle Text
    deriving (Show, Read, Eq, Ord)

moduleTxt :: Module -> Text
moduleTxt m = case m of
    URL t -> t
    SysPath t -> t
    CodaBundle t -> t

class (Monad m) => LoadModule m where
    loadModule :: Module -> (ByteString -> m a) -> m a
    parseError :: String -> m a
