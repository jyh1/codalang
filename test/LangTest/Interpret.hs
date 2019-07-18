{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LangTest.Interpret(testInterpret, testInterpretWIntrfc, CodaTestRes(..), CmdLog, makeDir, testInterpretWJRes) where

-- a dummy interpreter for CodaVal for testing purpose

import RIO hiding (to, view, traceShow, over)
import Control.Lens
import qualified RIO.Text as T
import Control.Monad.State
import qualified RIO.Map as M
import RIO.List (foldl)

import Lang.Types
import Lang.Fold
import Lang.Interpret
import Lang.JLang

data CodaTestRes = BunRes Text 
    | RunRes [(Text, CodaTestRes)] [CodaTestRes] 
    | DirRes CodaTestRes [Text] 
    | StrRes Text 
    | DictRes (Map Text CodaTestRes)
    | MakeRes [(Text, CodaTestRes)] [(Text, CodaTestRes)]
    | ResLam TypeDict (TextMap CodaTestRes) (TextMap CodaTestRes) CodaVal
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
    lit t = return (BunRes (tshow t))
    str = return . StrRes
    var v = use (envL . at v . to (fromMaybe errmsg))
        where
            errmsg = error ("Undefined var in test interpreter: " ++ T.unpack v)
    cl optVals clcmd = do
        cmd <- sequenceA clcmd
        let execcmd = case cmd of
                Run cmd' -> do
                    oe <- makeLog Nothing (LogRun cmd')
                    return (RunRes oe cmd')
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
                    StrRes s -> return (BunRes s)
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
                rest -> return val
            -- softConvert :: CodaTestRes -> CodaType -> CodaTestRes
            -- softConvert res ty = case ty of
            --     TypeString -> res
            --     TypeBundle -> res
            --     TypeRecord dt -> case res of
            --         DictRes dr -> DictRes (M.intersectionWith softConvert dr dt)
            --         _ -> DictRes (M.mapWithKey (\k ty -> softConvert (makeDir res k) ty) dt)
    dict dm = DictRes <$> (sequence dm)

    lambda args body = do
        clo <- use envL
        opt <- use optionvars
        return (ResLam args clo opt body)
    apply f args = sandBox $ do
        case f of
            ResLam argType clo opt body -> do
                envL .= M.union (M.intersection args argType) clo
                oldopt <- use optionvars
                optionvars .= opt
                res <- foldCoda body
                optionvars .= oldopt
                return res
            _ -> error (show f)

makeDir :: CodaTestRes -> Text -> CodaTestRes
makeDir val sub =
    case val of
        DictRes d -> fromMaybe keepDirRes (M.lookup sub d)
        DirRes v subs -> DirRes v (subs ++ [sub])
        _ -> DirRes val [sub]
        where
            keepDirRes = error "Undefined key in record"

runCatTxt opt val = do
    oe <- makeLog opt (LogCat val)
    return (tshow (oe, val))

runCat opt v = StrRes <$> runCatTxt opt v

makeLog opt cmd = case opt of
    Nothing -> do
        oe <- use (optionvars . to M.toList)
        cmdlog %= ((oe, cmd) :)
        return oe
    Just oe -> cmdlog %= ((oe, cmd) :) >> return oe

-- runMake :: () -> [(Text, CodaTestRes)] -> InterApp CodaTestRes
runMake opt ds = do
    oe <- makeLog opt (LogMake ds)
    return (MakeRes oe ds)
-- return logs of runned command and final result
type TestInterpret = CodaVal -> ([([(Text, CodaTestRes)], CmdLog CodaTestRes)], CodaTestRes)

testInterpret :: TestInterpret
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 mempty))

parseEle :: (TextMap CodaTestRes) -> CMDEle Text CodaTestRes -> CodaTestRes
parseEle deps ele
    -- | traceShow eleDep False = undefined 
    -- | length paths == 1 = StrRes ele
    | otherwise = case ele of
        Plain t -> t
        BundleRef eleVar ps -> 
            let eleDep = view (at eleVar . to (fromMaybe undefined)) deps in 
                fromDirRes eleDep ps

-- use interpret interface (after RCO)
instance Exec InterApp CodaTestRes where
    clRun opts deps cmd = do
        cmdlog %= ((_clOpt opts, LogRun resCmd) :)
        -- c <- getCounter
        return (RunRes (_clOpt opts) resCmd)
        where
            resCmd = parseEle deps <$> cmd

                            
    clLit _ u = return (BunRes (tshow u))
    clCat opts v = StrRes <$> runCatTxt (Just (_clOpt opts)) v
    clMake opts ks = runMake (Just (_clOpt opts)) ks
    strLit s = return (StrRes s)
    fromBundleName (StrRes s) = return (BunRes s)
    execDir d p = return (fromDirRes d [p])
    execRec dict = return (DictRes dict)

fromDirRes tres elePath
    | null elePath = tres
    | otherwise = case tres of
        DirRes r sub -> DirRes r (sub ++ elePath)
        _ -> DirRes tres elePath


testInterpretWIntrfc :: TestInterpret
testInterpretWIntrfc cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = evalCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 mempty))


testInterpretWJRes :: CodaType -> TestInterpret
testInterpretWJRes t v = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = fromJRes t (compileJ v)
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0 mempty))

fromJRes :: CodaType -> (JRes, [JBlock JRes]) -> InterApp CodaTestRes
fromJRes t (res, blks) = do
    mapM evalBlk blks
    evalResJRes t res
evalBlk :: JBlock JRes -> InterApp ()
evalBlk (JBlock v opt cmd) = do
    optVal <- (traverse . _2) evalStrJRes opt
    res <- case cmd of
        JCat r -> do
            tres <- evalBunJRes r
            StrRes <$> runCatTxt (Just optVal) tres
        JMake ts -> do
            tres <- (traverse . _2) evalBunJRes ts
            runMake (Just optVal) tres
        JRun deps cmds -> do
            cmdRes <- mapM fromCMDEle cmds
            depRes <- mapM evalBunJRes (M.fromList deps)
            let resCmd = parseEle depRes <$> cmdRes
            cmdlog %= ((optVal, LogRun resCmd) :)
            return (RunRes optVal resCmd)
        JLit v -> evalBunJRes v
    envL . at v ?= res

fromCMDEle :: CMDEle Text JRes -> InterApp (CMDEle Text CodaTestRes)
fromCMDEle e = case e of
    Plain t -> Plain <$> evalStrJRes t
    BundleRef b ps -> return (BundleRef b ps)
    

evalStrJRes :: JRes -> InterApp CodaTestRes
evalStrJRes r = case r of
    JVerbatim t -> return (StrRes t)
    JVariable v -> use (envL . at v . to (fromMaybe undefined))

evalBunJRes :: JRes -> InterApp CodaTestRes
evalBunJRes r = case r of
    JVerbatim t -> return (BunRes t)
    JVariable v -> do
        res <- use (envL . at v . to (fromMaybe undefined))
        return $ case res of
            StrRes k -> BunRes k
            _ -> res
    JDir d ps -> (\d -> fromDirRes d ps) <$> evalBunJRes d

evalResJRes :: CodaType -> JRes -> InterApp CodaTestRes
evalResJRes TypeString r = evalStrJRes r
evalResJRes TypeBundle r = evalBunJRes r
evalResJRes (TypeRecord d) r = case r of
    JRec rm -> DictRes <$> sequence (M.intersectionWith evalResJRes d rm)
    _ -> evalBunJRes r