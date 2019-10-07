{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang.TypeCheck(typeCheck) where

import           Control.Monad.State
import           RIO                     hiding ( to, view, over )
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
    | NotFunction CodaType
    | Incompat {expected :: CodaType, current :: CodaType}
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
        NotFunction ty -> T.unlines
            ["Expected function, actual type:", tshow ty, " in: ", astStr]
        Incompat{} -> T.unlines ["Incompatible argument type in: ", astStr]
        where astStr = pprintCoda ast


type TCState = Map VarName (TCRes CodaVal)
type TCPass = StateT TCState (Either TypeError)
data TCRes a = TCRes {resType :: CodaType, resVal :: a, resOrig :: a}
    deriving (Show, Read, Eq, Functor)
fmapT :: CodaType -> (a -> b) -> TCRes a -> TCRes b
fmapT t f res = (fmap f res){resType = t}
liftRes2 :: (a -> b -> c) -> TCRes a -> TCRes b -> TCRes c
liftRes2 f (TCRes _ v1 o1) (TCRes t2 v2 o2) = TCRes t2 (f v1 v2) (f o1 o2)
coSequenceT :: (Functor f) => CodaType -> f (TCRes a) -> TCRes (f a)
coSequenceT t as = TCRes {resType = t, resVal = resVal <$> as, resOrig = resOrig <$> as}

makeRes :: CodaType -> CodaVal -> (TCRes CodaVal)
makeRes t v = TCRes t v v

instance LocalVar TCState TCPass (TCRes CodaVal)

throwErr :: TypeError -> TCPass a
throwErr err = lift (Left err)

expectType :: CodaType -> CodaType -> CodaVal -> TCPass ()
expectType exp got ast
        | exp /= got = throwErr (TypeError (Mismatch exp got) ast)
        | otherwise = return ()

instance CodaLangEnv TCPass (TCRes CodaVal) where
    lit u = return (makeRes TypeBundle (Lit u))
    var vn = do
        varVal <- use (envL . at vn)
        maybe err return varVal
      where
        err = throwErr (TypeError UnDef (Var vn))
    str k = return (makeRes TypeString (Str k))
    cl opt r@(Run es) = do
        opt' <- (traverse . cmdExpr) id opt
        _ <- (traverse . cmdExpr) isString opt'
        r' <- sequence r
        mapM_ isCmdEle r'
        let optVals = over (traverse . cmdExpr) resVal opt'
        return (Cl optVals <$> (coSequenceT TypeBundle r'))
        where
            isCmdEle t = case resType t of
                -- bundle or string or record of bundle
                ty@TypeLam{} -> throwErr (TypeError (Mismatch TypeBundle ty) (resOrig t))
                TypeRecord tr -> mapM_ (isBundle (resOrig t)) tr
                _ -> return ()
            isBundle orig t = case t of
                TypeBundle -> return ()
                _ -> throwErr (TypeError (Mismatch TypeBundle t) orig)
            isString t = case resType t of
                TypeString -> return ()
                ty -> throwErr (TypeError (Mismatch TypeString ty) (resOrig t))
    cl _ (ClCat _) = error "Cat command during type check"
    dir val sub = case resType val of
        TypeString -> throwErr (TypeError (Mismatch TypeBundle TypeString) ast)
        TypeBundle -> return (tagType TypeBundle)
        TypeRecord dic -> do
            let dicEle = (return . tagType) <$> M.lookup sub dic
            fromMaybe (throwErr (TypeError (KeyError sub) (Dir ast sub))) dicEle
        where
            ast = resOrig val
            tagType t = fmapT t (`Dir` sub) val
    clet vn val body = do
        valRes <- val
        bodyRes <- withVar vn (Var vn <$ valRes) body
        return (liftRes2 (Let vn) valRes bodyRes)

    -- deprecated
    convert _ val vt = bool castErr doConvert isConvertable
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
    
    lambda args body = sandBox $ do
        let argEnv = M.mapWithKey (\k t -> makeRes t (Var k)) args
        envL %= M.union argEnv
        bodyT <- foldCoda body
        let lamty = TypeLam args (resType bodyT)
        return (fmapT lamty (Lambda args) bodyT)

    apply fun arg = case (resType fun) of
        TypeLam ad rt -> 
            case argty `isSubtypeOf` funarg of
                True -> return applyNode
                _ -> throwErr (TypeError (Incompat funarg argty) (resOrig applyNode))
            where
                funarg = TypeRecord ad
                argty = TypeRecord (resType <$> arg)
                applyNode = (liftRes2 Apply fun (coSequenceT rt arg))
        _ -> throwErr (TypeError (NotFunction (resType fun)) (resOrig fun))



typeCheck :: CodaVal -> Either Text (CodaType, CodaVal)
typeCheck cv = case res of
    Right val -> Right (resType val, resVal val)
    Left  err     -> Left (tshow err)
  where
    app = foldCoda cv :: TCPass (TCRes CodaVal)
    res = evalStateT app mempty
