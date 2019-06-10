{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang.TypeCheck(typeCheck) where

import           Control.Monad.State
import           RIO                     hiding ( to )
import           Control.Lens
import qualified RIO.Text                      as T
import qualified RIO.Map                       as M
import           RIO.List                       ( unzip )

import           Lang.Types
import           Lang.Fold
import           Lang.PPrint                    ( pprintCoda )

data TypeErrorInfo = UnDef
    | Mismatch {expected :: CodaType, current :: CodaType}
    | KeyError {usedKey :: Text}
    | TypeCastError {origin :: CodaType, target :: CodaType}
    deriving (Read, Eq, Typeable)

data TypeError = TypeError TypeErrorInfo CodaVal
    deriving (Read, Eq, Typeable)
instance Exception TypeError

instance Show TypeError where
    show (TypeError inf ast) = T.unpack $ case inf of
        UnDef            -> "Undefined variable: " <> astStr
        Mismatch ex curr -> T.unlines
            [ "Expected type: " <> tshow ex
            , "Actual type: " <> tshow curr
            , "In the expression: "
            , astStr
            ]
        KeyError k -> T.unlines
            [ "Undefined key: " <> k
            , "In the expression:"
            , astStr
            ]
        TypeCastError o t -> T.unlines
            [ "Unable to cast type:"
            , tshow o
            , "to type:"
            ,  tshow t
            ]
        where astStr = pprintCoda ast


type TCState = Map VarName CodaType
type TCPass = StateT TCState (Either TypeError)
data TCRes a = TCRes {resType :: CodaType, resVal :: a, resOrig :: a}
    deriving (Show, Read, Eq, Functor)
fmapT :: CodaType -> (a -> b) -> TCRes a -> TCRes b
fmapT t f res = (fmap f res){resType = t}
liftRes2 :: (a -> b -> c) -> TCRes a -> TCRes b -> TCRes c
liftRes2 f (TCRes _ v1 o1) (TCRes t2 v2 o2) = TCRes t2 (f v1 v2) (f o1 o2)
coSequenceT :: CodaType -> [TCRes a] -> TCRes [a]
coSequenceT t as = TCRes {resType = t, resVal = resVal <$> as, resOrig = resOrig <$> as}

makeRes :: CodaType -> CodaVal -> (TCRes CodaVal)
makeRes t v = TCRes t v v

instance LocalVar TCState TCPass CodaType

throwErr :: TypeError -> TCPass a
throwErr err = lift (Left err)

instance CodaLangEnv TCPass (TCRes CodaVal) where
    lit u = return (makeRes typeBundle (Lit u))
    var vn = do
        varType <- use (envL . at vn)
        maybe err getRes varType
      where
        err = throwErr (TypeError UnDef (Var vn))
        getRes t = return (makeRes t (Var vn))
    str k = return (makeRes TypeString (Str k))
    cl (Run es) = do
        es' <- sequence es
        return (makeRun <$> (coSequenceT typeBundle es'))
        where
            makeRun = Cl . Run
    cl (ClCat _) = error "Cat command during type check"
    dir val sub = case resType val of
        TypeString -> throwErr (TypeError (Mismatch typeBundle TypeString) ast)
        BundleDic d -> case d of
            TAll -> return (tagType typeBundle)
            TDict dic -> do
                let dicEle = (return . tagType) <$> M.lookup sub dic
                fromMaybe (throwErr (TypeError (KeyError sub) ast)) dicEle
        where
            ast = resOrig val
            tagType t = fmapT t (`Dir` sub) val
    clet vn val body = do
        valRes <- val
        bodyRes <- withVar vn (resType valRes) body
        return (liftRes2 (Let vn) valRes bodyRes)
    convert val vt
        | valType == vt = return val
        | convertable valType vt = return (fmapT vt (`Convert` vt) val)
        | otherwise = throwErr (TypeError (TypeCastError valType vt) (resOrig val))
        where
            valType = resType val
            convertable t1 t2 = case (t1, t2) of
                (TypeString, _) -> True
                (_, TypeString) -> True
                (BundleDic d1, BundleDic d2) -> d2 `isSubsetOf` d1
            isSubsetOf d1 d2 = case (d1, d2) of
                (_, TAll) -> True
                (TAll, _) -> False
                (TDict td1, TDict td2) -> M.null (td1 `M.difference` td2)
    dict d = do
        let td = resType <$> d
            tv = resVal <$> d
            torig = resOrig <$> d
        return (TCRes {resType = BundleDic (TDict td), resVal = Dict tv, resOrig = Dict torig})

typeCheck :: CodaVal -> Either Text (CodaType, CodaVal)
typeCheck cv = case res of
    Right val -> Right (resType val, resVal val)
    Left  err     -> Left (tshow err)
  where
    app = foldCoda cv :: TCPass (TCRes CodaVal)
    res = evalStateT app mempty
