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


type TCState = Map VarName (TCRes CodaVal)
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

instance LocalVar TCState TCPass (TCRes CodaVal)

throwErr :: TypeError -> TCPass a
throwErr err = lift (Left err)

instance CodaLangEnv TCPass (TCRes CodaVal) where
    lit u = return (makeRes TypeBundle (Lit u))
    var vn = do
        varVal <- use (envL . at vn)
        maybe err return varVal
      where
        err = throwErr (TypeError UnDef (Var vn))
    str k = return (makeRes TypeString (Str k))
    cl (Run es) = do
        es' <- sequence es
        mapM_ notRecord es'
        return (makeRun <$> (coSequenceT TypeBundle es'))
        where
            makeRun = Cl . Run
            notRecord t = case resType t of
                ty@TypeRecord{} -> throwErr (TypeError (Mismatch TypeBundle ty) (resOrig t))
                _ -> return ()
    cl (ClCat _) = error "Cat command during type check"
    dir val sub = case resType val of
        TypeString -> throwErr (TypeError (Mismatch TypeBundle TypeString) ast)
        TypeBundle -> return (tagType TypeBundle)
        TypeRecord dic -> do
            let dicEle = (return . tagType) <$> M.lookup sub dic
            fromMaybe (throwErr (TypeError (KeyError sub) ast)) dicEle
        where
            ast = resOrig val
            tagType t = fmapT t (`Dir` sub) val
    clet vn val body = do
        valRes <- val
        bodyRes <- withVar vn (Var vn <$ valRes) body
        return (liftRes2 (Let vn) valRes bodyRes)
    convert _ val vt 
        = case (ty, vt) of
            (TypeRecord{}, TypeString) -> castErr
            (TypeString, TypeRecord{}) -> castErr
            (TypeRecord{}, TypeBundle) -> doConvert
            (TypeRecord {}, TypeRecord {}) -> bool castErr doConvert isConvertable
            _ -> doConvert
        where
            ty = resType val
            ast = resOrig val
            castErr = throwErr (TypeError (TypeCastError ty vt) ast)
            doConvert = return $ fmapT vt (\v -> Convert (Just ty) v vt) val
            isConvertable = ty `convertable` vt
    dict dm = do
        d <- sequence dm
        let td = resType <$> d
            tv = resVal <$> d
            torig = resOrig <$> d
            ty = TypeRecord td
        return (TCRes {resType = ty, resVal = Dict tv, resOrig = Dict torig})

typeCheck :: CodaVal -> Either Text (CodaType, CodaVal)
typeCheck cv = case res of
    Right val -> Right (resType val, resVal val)
    Left  err     -> Left (tshow err)
  where
    app = foldCoda cv :: TCPass (TCRes CodaVal)
    res = evalStateT app mempty
