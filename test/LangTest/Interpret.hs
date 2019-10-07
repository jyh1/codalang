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
    | RunRes [CodaTestRes] [CodaTestRes] 
    | DirRes CodaTestRes [Text] 
    | StrRes Text 
    | DictRes (Map Text CodaTestRes)
    | MakeRes [CodaTestRes] [(Text, CodaTestRes)]
    | ResLam TypeDict (TextMap CodaTestRes) CodaVal
    deriving (Show, Read, Eq, Ord)

data CmdLog a = LogRun [a] | LogCat a | LogMake [(Text, a)]
    deriving (Show, Read, Eq, Ord)

type OptionEnv a = TextMap a

data CodaInterEnv a = CodaInterEnv {
      _envmap :: VarMap a
    , _cmdlog :: [([CodaTestRes], CmdLog a)]
    , _counter :: Int
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

evalCmdEle :: CMDEle CodaTestRes Text -> CodaTestRes
evalCmdEle = fromCMDEle id StrRes

evalRunCmdEle :: CMDEle CodaTestRes Text -> [CodaTestRes]
evalRunCmdEle = fromCMDEle rmDict (\t -> [StrRes t])
    where
        rmDict (DictRes _) = [StrRes ""]
        rmDict other = [other]

instance CodaLangEnv InterApp CodaTestRes where
    lit t = return (BunRes (tshow t))
    str = return . StrRes
    var v = use (envL . at v . to (fromMaybe errmsg))
        where
            errmsg = error ("Undefined var in test interpreter: " ++ T.unpack v)
    cl optVals clcmd = do
        cmd <- sequenceA clcmd
        optEnvs <- (traverse . cmdExpr) id optVals
        let optEles = evalCmdEle <$> optEnvs
            execcmd = case cmd of
                Run cmd' -> do
                    let cmdEles = concatMap evalRunCmdEle cmd'
                    oe <- makeLog optEles (LogRun cmdEles)
                    return (RunRes oe cmdEles)
                ClCat val -> runCat optEles val
                ClMake val -> runMake optEles val
        res <- execcmd
        return res
    dir val sub = return $ makeDir val sub
    clet varn val body = do
        valres <- val
        withVar varn valres body
            -- OptionVar varn -> do
            --     origin <- use optionvars
            --     optionvars %= (M.insert varn valres)
            --     res <- body
            --     optionvars .= origin
            --     return res
        
    -- convert tt val vt = makeConvert val vt
    --     where
    --         makeConvert :: CodaTestRes -> CodaType -> InterApp CodaTestRes
    --         makeConvert val vt = case vt of
    --             TypeString -> case val of
    --                 StrRes{} -> return val
    --                 _ -> runCat Nothing val
    --             TypeBundle -> case val of
    --                 StrRes s -> return (BunRes s)
    --                 DictRes dict -> do
    --                     resD <- mapM (`makeConvert` TypeBundle) dict
    --                     runMake Nothing (M.toList resD)
    --                 _ -> return val
    --             TypeRecord d -> case val of
    --                 DictRes vd -> DictRes <$> (sequence $
    --                     M.intersectionWith (\v t -> makeConvert v t) vd d)
    --                 _ -> DictRes <$> sequence (M.mapWithKey (\k t -> dir val k >>= (`makeConvert` t)) d)
    --                 where
    --                     hasTypeString :: CodaType -> Bool
    --                     hasTypeString t = case t of
    --                         TypeString -> True
    --                         TypeBundle -> False
    --                         TypeRecord d -> anyOf traverse hasTypeString d
    --             rest -> return val
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
        return (ResLam args clo body)
    apply f args = sandBox $ do
        case f of
            ResLam argType clo body -> do
                envL .= M.union (M.intersection args argType) clo
                res <- foldCoda body
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

makeLog oe cmd = cmdlog %= ((oe, cmd) :) >> return oe

-- runMake :: () -> [(Text, CodaTestRes)] -> InterApp CodaTestRes
runMake opt ds = do
    oe <- makeLog opt (LogMake ds)
    return (MakeRes oe ds)
-- return logs of runned command and final result
type TestInterpret = CodaVal -> ([([CodaTestRes], CmdLog CodaTestRes)], CodaTestRes)

testInterpret :: TestInterpret
testInterpret cv = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = foldCoda cv
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0))

parseEle :: (TextMap CodaTestRes) -> CodaCMDEle CodaTestRes -> CodaTestRes
parseEle deps ele
    -- | traceShow eleDep False = undefined 
    -- | length paths == 1 = StrRes ele
    | otherwise = case ele of
        TextValue t -> t
        TextPlain t -> StrRes t
        BundleRef eleVar -> 
            let eleDep = view (at eleVar . to (fromMaybe undefined)) deps in 
                eleDep

-- use interpret interface (after RCO)
instance Exec InterApp CodaTestRes where
    clRun (ClInfo vn opt) deps cmd = do
        let optval = parseEle mempty <$> opt
            resCmd = parseEle deps <$> cmd
        cmdlog %= ((optval, LogRun resCmd) :)
        return (RunRes optval resCmd)
                            
    clLit _ u = return (BunRes (tshow u))
    clCat (ClInfo _ optval) v = StrRes <$> runCatTxt undefined v
    clMake (ClInfo _ optval) ks = runMake undefined ks
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
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0))


testInterpretWJRes :: CodaType -> TestInterpret
testInterpretWJRes t v = (_cmdlog env, res)
    where
        app :: InterApp CodaTestRes
        app = fromJRes t (compileJ v)
        (res, env) = runIdentity (runStateT app (CodaInterEnv mempty [] 0))

fromJRes :: CodaType -> (JRes, [JBlock JRes]) -> InterApp CodaTestRes
fromJRes t (res, blks) = do
    mapM evalBlk blks
    evalResJRes t res
evalBlk :: JBlock JRes -> InterApp ()
evalBlk (JBlock v opt cmd) = do
    optRes <- traverse fromCMDJEle opt
    let optVal = parseEle mempty <$> optRes
    res <- case cmd of
        JCat r -> do
            tres <- evalBunJRes r
            StrRes <$> runCatTxt optVal tres
        JMake ts -> do
            tres <- (traverse . _2) evalBunJRes ts
            runMake optVal tres
        JRun deps cmds -> do
            cmdRes <- mapM fromCMDJEle cmds
            depRes <- mapM evalBunJRes (M.fromList deps)
            let resCmd = parseEle depRes <$> cmdRes
            cmdlog %= ((optVal, LogRun resCmd) :)
            return (RunRes optVal resCmd)
        JLit v -> return (BunRes v)
    envL . at v ?= res

fromCMDJEle :: CodaCMDEle JRes -> InterApp (CodaCMDEle CodaTestRes)
fromCMDJEle e = case e of
    TextValue t -> TextValue <$> evalStrJRes t
    BundleRef b -> return (BundleRef b)
    TextPlain t -> return (TextPlain t)
    

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