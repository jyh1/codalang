{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LangTest.Interpret(testInterpret, testInterpretWIntrfc, CodaTestRes(..), CmdLog, makeDir) where

-- a dummy interpreter for CodaVal for testing purpose

import RIO hiding (to, view, traceShow, over)
import Control.Lens
import qualified RIO.Text as T
import Control.Monad.State
import RIO.List.Partial (head, tail)
import qualified RIO.Map as M
import Debug.Trace (traceShow)

import Lang.Types
import Lang.Fold
import Lang.Interpret

data CodaTestRes = BunRes UUID 
    | RunRes Int 
    | DirRes CodaTestRes [Text] 
    | StrRes Text 
    | DictRes (Map Text CodaTestRes)
    | MakeRes Int
    deriving (Show, Read, Eq, Ord)

data CmdLog a = LogRun [a] | LogCat a | LogMake [(Text, a)]
    deriving (Show, Read, Eq, Ord)

type OptionEnv a = [(Text, a)]

data CodaInterEnv a = CodaInterEnv {
      _envmap :: VarMap a
    , _cmdlog :: [(OptionEnv a, CmdLog a)]
    , _counter :: Int
    , _optionvars :: OptionEnv a
    }
    deriving (Show, Read, Eq)
makeLenses ''CodaInterEnv

type InterApp = StateT (CodaInterEnv CodaTestRes) Identity

instance HasEnv (CodaInterEnv a) a where
    envL = envmap

instance HasCounter (CodaInterEnv a) where
    counterL = counter
instance GetCounter (CodaInterEnv CodaTestRes) InterApp

instance LocalVar (CodaInterEnv CodaTestRes) InterApp CodaTestRes

instance CodaLangEnv InterApp CodaTestRes where
    lit = return . BunRes
    str = return . StrRes
    var v = use (envL . at v . to (fromMaybe errmsg))
        where
            errmsg = error ("Undefined var in test interpreter: " ++ T.unpack v)
    cl _ clcmd = do
        cmd <- sequenceA clcmd
        case cmd of
            Run cmd' -> do
                makeLog (LogRun cmd')
                RunRes <$> getCounter
            ClCat val -> runCat val
            ClMake val -> runMake val
    dir val sub = return $ makeDir val sub
    clet af val body = do
        valres <- val
        case af of
            Variable varn -> withVar varn valres body
            OptionVar varn -> do
                optionvars %= ((varn, valres):)
                res <- body
                optionvars %= tail
                return res
        
    convert tt val vt = makeConvert val vt
        where
            makeConvert :: CodaTestRes -> CodaType -> InterApp CodaTestRes
            makeConvert val vt = case vt of
                TypeString -> case val of
                    StrRes{} -> return val
                    _ -> runCat val
                TypeBundle -> case val of
                    StrRes s -> return (BunRes (BundleName s))
                    DictRes dict -> do
                        resD <- mapM (`makeConvert` TypeBundle) dict
                        runMake (M.toList resD)
                    _ -> return val
                TypeRecord d -> case val of
                    DictRes vd -> DictRes <$> (sequence $
                        M.intersectionWith (\v t -> makeConvert v t) vd d)
                    _ -> DictRes <$> sequence (M.mapWithKey (\k t -> dir val k >>= (`makeConvert` t)) d)
                    where
                        hasTypeString :: CodaType -> Bool
                        hasTypeString t = case t of
                            TypeString -> True
                            TypeBundle -> False
                            TypeRecord d -> anyOf traverse hasTypeString d
            -- softConvert :: CodaTestRes -> CodaType -> CodaTestRes
            -- softConvert res ty = case ty of
            --     TypeString -> res
            --     TypeBundle -> res
            --     TypeRecord dt -> case res of
            --         DictRes dr -> DictRes (M.intersectionWith softConvert dr dt)
            --         _ -> DictRes (M.mapWithKey (\k ty -> softConvert (makeDir res k) ty) dt)
    dict dm = DictRes <$> (sequence dm)

makeDir :: CodaTestRes -> Text -> CodaTestRes
makeDir val sub =
    case val of
        DictRes d -> fromMaybe keepDirRes (M.lookup sub d)
        DirRes v subs -> DirRes v (subs ++ [sub])
        _ -> DirRes val [sub]
        where
            keepDirRes = error "Undefined key in record"

runCatTxt :: CodaTestRes -> InterApp Text
runCatTxt val = do
    makeLog (LogCat val)
    (("catres" <>) . tshow) <$> getCounter

runCat v = StrRes <$> runCatTxt v

makeLog cmd = do
    oe <- use optionvars
    cmdlog %= ((oe, cmd) :)

runMake :: [(Text, CodaTestRes)] -> InterApp CodaTestRes
runMake ds = do
    makeLog (LogMake ds)
    MakeRes <$> getCounter
-- return logs of runned command and final result
type TestInterpret = CodaVal -> ([(OptionEnv CodaTestRes, CmdLog CodaTestRes)], CodaTestRes)

testInterpret :: TestInterpret
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 []))

-- use interpret interface (after RCO)
instance Exec InterApp CodaTestRes where
    clRun _ deps cmd = do
        cmdlog %= (([], LogRun resCmd) :)
        c <- getCounter
        return (RunRes c)
        where
            resCmd = parseEle <$> cmd
            parseEle ele
                -- | traceShow eleDep False = undefined 
                -- | length paths == 1 = StrRes ele
                | otherwise = case ele of
                    Plain t -> StrRes t
                    BundleRef eleVar ps -> 
                        let eleDep = view (at eleVar . to (fromMaybe undefined)) deps in 
                            fromDep eleDep ps
                            
    clLit _ u = return (BunRes u)
    clCat _ v = RuntimeString <$> runCatTxt (fromDep v [])
    clMake _ ks = runMake (over (traverse . _2) ( `fromDep` []) ks)

fromDep (Deps tres depPath) elePath
    | null ps = tres
    | otherwise = case tres of
        DirRes r sub -> DirRes r (sub ++ ps)
        _ -> DirRes tres ps
    where
        ps = depPath ++ elePath

-- catToBundle (CatRes i) = RunRes i
-- catToBundle (DirRes k p) = DirRes (catToBundle k) p
-- catToBundle v = v

testInterpretWIntrfc :: TestInterpret
testInterpretWIntrfc cv = (_cmdlog env, newRes res)
    where
        app :: InterApp (RuntimeRes CodaTestRes)
        app = evalCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 []))
        newRes res = case res of
            RuntimeString t -> StrRes t
            RuntimeBundle m ps -> case (m, ps) of
                (DirRes r rps, _) -> DirRes r (rps ++ ps)
                (other, []) -> other
                (other, ps) -> DirRes other ps
            RuntimeRecord ks -> DictRes (newRes <$> M.fromList ks)