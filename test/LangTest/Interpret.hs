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

type OptionEnv a = TextMap a

data CodaInterEnv a = CodaInterEnv {
      _envmap :: VarMap a
    , _cmdlog :: [([(Text, a)], CmdLog a)]
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
    cl optVals clcmd = do
        cmd <- sequenceA clcmd
        let execcmd = case cmd of
                Run cmd' -> do
                    makeLog Nothing (LogRun cmd')
                    RunRes <$> getCounter
                ClCat val -> runCat Nothing val
                ClMake val -> runMake Nothing val
        if null optVals then execcmd else 
            do
                optionvars .= M.fromList optVals
                res <- execcmd
                optionvars .= M.empty
                return res
    dir val sub = return $ makeDir val sub
    clet af val body = do
        valres <- val
        case af of
            Variable varn -> withVar varn valres body
            OptionVar varn -> do
                origin <- use optionvars
                optionvars %= (M.insert varn valres)
                res <- body
                optionvars .= origin
                return res
        
    convert tt val vt = makeConvert val vt
        where
            makeConvert :: CodaTestRes -> CodaType -> InterApp CodaTestRes
            makeConvert val vt = case vt of
                TypeString -> case val of
                    StrRes{} -> return val
                    _ -> runCat Nothing val
                TypeBundle -> case val of
                    StrRes s -> return (BunRes (BundleName s))
                    DictRes dict -> do
                        resD <- mapM (`makeConvert` TypeBundle) dict
                        runMake Nothing (M.toList resD)
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

-- runCatTxt :: CodaTestRes -> InterApp Text
runCatTxt opt val = do
    makeLog opt (LogCat val)
    (("catres" <>) . tshow) <$> getCounter

runCat opt v = StrRes <$> runCatTxt opt v

makeLog opt cmd = case opt of
    Nothing -> do
        oe <- use (optionvars . to M.toList)
        cmdlog %= ((oe, cmd) :)
    Just oe -> cmdlog %= ((oe, cmd) :)

-- runMake :: () -> [(Text, CodaTestRes)] -> InterApp CodaTestRes
runMake opt ds = do
    makeLog opt (LogMake ds)
    MakeRes <$> getCounter
-- return logs of runned command and final result
type TestInterpret = CodaVal -> ([([(Text, CodaTestRes)], CmdLog CodaTestRes)], CodaTestRes)

testInterpret :: TestInterpret
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 mempty))

-- use interpret interface (after RCO)
getOptEnv :: ClInfo CodaTestRes -> [(Text, CodaTestRes)]
getOptEnv o = over (traverse . _2) (fromRTRes) (_clOpt o)
    where
        fromRTRes (RuntimeString t) = StrRes t
instance Exec InterApp CodaTestRes where
    clRun opts deps cmd = do
        cmdlog %= ((getOptEnv opts, LogRun resCmd) :)
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
    clCat opts v = RuntimeString <$> runCatTxt (Just (getOptEnv opts)) (fromDep v [])
    clMake opts ks = runMake (Just (getOptEnv opts)) (over (traverse . _2) ( `fromDep` []) ks)

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
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 mempty))
        newRes res = case res of
            RuntimeString t -> StrRes t
            RuntimeBundle m ps -> case (m, ps) of
                (DirRes r rps, _) -> DirRes r (rps ++ ps)
                (other, []) -> other
                (other, ps) -> DirRes other ps
            RuntimeRecord ks -> DictRes (newRes <$> M.fromList ks)