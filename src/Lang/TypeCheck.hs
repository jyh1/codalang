{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.TypeCheck(typeCheck) where

import           Control.Monad.State
import           RIO                     hiding ( to )
import           Control.Lens
import qualified RIO.Text                      as T
import           RIO.List                       ( unzip )

import           Lang.Types
import           Lang.Fold
import           Lang.PPrint                    ( pprintCoda )

data TypeErrorInfo = UnDef
    | Mismatch {expected :: CodaType, current :: CodaType}
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
        where astStr = pprintCoda ast


type TCState = Map VarName CodaType
type TCPass = StateT TCState (Either TypeError)
type TCRes = (CodaType, CodaVal)

instance LocalVar TCState TCPass CodaType

throwErr :: TypeError -> TCPass a
throwErr err = lift (Left err)

instance CodaLangEnv TCPass TCRes where
    lit u = return (typeBundle, Lit u)
    var vn = do
        varType <- use (envL . at vn)
        maybe err getRes varType
      where
        err = throwErr (TypeError UnDef (Var vn))
        getRes t = return (t, Var vn)
    str k = return (TypeString, Str k)
    cl (Run es) = do
        es' <- sequence es
        let (_, elems) = unzip es'
        return (typeBundle, Cl (Run elems))
    dir val sub = case val of
        (TypeString, ast) ->
            throwErr (TypeError (Mismatch typeBundle TypeString) ast)
        (BundleDic{}, ast) -> return (typeBundle, Dir ast sub)
    clet vn val body = do
        (valt , valast ) <- val
        (bodyT, bodyAst) <- withVar vn valt body
        return (bodyT, Let vn valast bodyAst)
    convert val vt = return (vt, Convert (snd val) vt)

typeCheck :: CodaVal -> Either Text CodaType
typeCheck cv = case res of
    Right (ty, _) -> Right ty
    Left  err     -> Left (tshow err)
  where
    app = foldCoda cv :: TCPass TCRes
    res = evalStateT app mempty
