{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lang.Types where

import           RIO hiding (over)
import qualified RIO.Text as T
import qualified RIO.Map as M
import           Control.Lens hiding ((.=))
import Data.Aeson

-- UUID of CodaLab bundle
data UUID = UUID Text | BundleName Text
    deriving (Eq, Ord, Read, Generic)
instance FromJSON UUID
instance ToJSON UUID where
    toEncoding = genericToEncoding defaultOptions


instance Show UUID where
    show (UUID n) = "0x" ++ T.unpack n
    show (BundleName t) = T.unpack t

type VarName = Text

-- Codalab command

data Cmd a = Run [CMDEle a Text] | ClCat a | ClMake [(Text, a)]
    deriving (Eq, Ord, Read, Show, Generic)
instance Functor Cmd where
    fmap f cmd = case cmd of
        ClCat a -> ClCat (f a)
        ClMake xs -> ClMake (fmap (fmap f) xs)
        Run xs -> Run (over (traverse . cmdExpr) f xs)
instance Foldable Cmd where
    foldMap f (ClCat a) = f a
    foldMap f (Run as) = foldMapOf (traverse . cmdExpr) f as
    foldMap f (ClMake rs) = foldMapOf (traverse . _2) f rs
instance Traversable Cmd where
    traverse f (ClCat a) = ClCat <$> (f a)
    traverse f (Run as) = Run <$> ((traverse . cmdExpr) f as)
    traverse f (ClMake rs) = ClMake <$> ((traverse . _2) f rs)
instance (FromJSON a) => FromJSON (Cmd a) where
instance (ToJSON a) => ToJSON (Cmd a) where
    toEncoding = genericToEncoding defaultOptions


type CodaCmd = Cmd CodaVal

-- Command options
type Env = [(Text, Text)]

-- assign to let
data AssignForm = Variable VarName
    | OptionVar VarName
    deriving (Eq, Ord, Read, Show, Generic)
instance ToJSON AssignForm where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AssignForm

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
    deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON CodaVal
instance ToJSON CodaVal where
    toEncoding = genericToEncoding defaultOptions

makeCl :: CodaCmd -> CodaVal
makeCl = Cl []

defConvert :: CodaVal -> CodaType -> CodaVal
defConvert = Convert Nothing

tmpName :: Text
tmpName = "codalang"

type TextMap = Map Text
type TypeDict = TextMap CodaType
data CodaType = TypeString | TypeBundle | TypeRecord TypeDict | TypeLam TypeDict CodaType
    deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON CodaType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CodaType where

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

data Execute = ExecRun [(Text, Text)] String ClOption
    | ExecCat Text ClOption
    | ExecMake [(Text, Text)] ClOption
    deriving (Show, Read, Eq)

buildPath :: [Text] -> Text
buildPath = T.intercalate "/"

data CMDEle a b = CMDExpr a | Plain b 
    deriving (Show, Read, Eq, Ord)

fromCMDEle :: (a -> c) -> (b -> c) -> (CMDEle a b) -> c
fromCMDEle cmd plain e = case e of
    CMDExpr a -> cmd a
    Plain b -> plain b

cmdExpr :: Prism (CMDEle a c) (CMDEle b c) a b
cmdExpr = prism CMDExpr extract
    where
        extract e = case e of
            CMDExpr a -> Right a
            Plain b -> Left (Plain b)

tagType :: (KeyValue kv) => Text -> kv
tagType t = "type" .= t
tagContent :: (KeyValue kv, ToJSON a) => a -> kv
tagContent t = "content" .= t
doEncode :: [Series] -> Encoding
doEncode ts = pairs (mconcat ts)
makeCmdEleKV :: (KeyValue kv, ToJSON a, ToJSON b) => CMDEle a b -> [kv]
makeCmdEleKV x = case x of
    Plain b -> [tagContent b, tagType "plain"]
    CMDExpr r -> [tagContent r, tagType "expr"]
instance (ToJSON a, ToJSON b) => ToJSON (CMDEle a b) where
    toJSON e = object (makeCmdEleKV e)
    toEncoding e = doEncode (makeCmdEleKV e)
instance (FromJSON a, FromJSON b) => FromJSON (CMDEle a b) where
    parseJSON o@(Object v) = do
        ty :: Text <- v .: "type"
        cont :: Value <- v .: "content"
        case ty of
            "plain" -> Plain <$> parseJSON cont
            "expr" -> CMDExpr <$> parseJSON cont
            _ -> fail ("Unkonw type in CMDEle: " ++ T.unpack ty)
    parseJSON invalid = fail ("Error parsing CMDEle: " ++ show invalid)

data CodaCMDEle a = BundleRef Text | TextValue a | TextPlain Text
    deriving (Show, Read, Eq, Ord)

makeCodaCmdEleKV :: (KeyValue kv, ToJSON a) => CodaCMDEle a -> [kv]
makeCodaCmdEleKV x = case x of
    TextPlain b -> [tagContent b, tagType "plain"]
    BundleRef r -> [tagContent r, tagType "bundle"]
    TextValue a -> [tagContent a, tagType "quote"]
instance (ToJSON a) => ToJSON (CodaCMDEle a) where
    toJSON e = object (makeCodaCmdEleKV e)
    toEncoding e = doEncode (makeCodaCmdEleKV e)
instance (FromJSON a) => FromJSON (CodaCMDEle a) where
    parseJSON o@(Object v) = do
        ty :: Text <- v .: "type"
        cont :: Value <- v .: "content"
        case ty of
            "plain" -> TextPlain <$> parseJSON cont
            "bundle" -> BundleRef <$> parseJSON cont
            "quote" -> TextValue <$> parseJSON cont
            _ -> fail ("Unkonw type in CMDEle: " ++ T.unpack ty)
    parseJSON invalid = fail ("Error parsing CodaCMDEle: " ++ show invalid)

fromCodaCMDEle :: (Text -> c) -> (a -> c) -> CodaCMDEle a -> c
fromCodaCMDEle bundleref verbatim e = case e of
    BundleRef t -> bundleref t
    TextValue v -> verbatim v

class (Monad m) => Exec m a where
    clRun :: (ClInfo a) -> TextMap a -> [CodaCMDEle a] -> m a
    clCat :: (ClInfo a) -> a -> m a
    clLit :: Text -> UUID -> m a
    clMake :: (ClInfo a) -> [(Text, a)] -> m a
    strLit :: Text -> m a
    fromBundleName :: a -> m a
    execDir :: a -> Text -> m a
    execRec :: TextMap a -> m a


data ClInfo a = ClInfo {_codaName :: Text, _clOpt :: [(Text, a)]}
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
