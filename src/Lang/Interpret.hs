{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- interpreter for codaval, after RCO
module Lang.Interpret(evalCoda) where

import           RIO
import qualified RIO.Text                      as T
import           RIO.List                       ( unzip )
import qualified RIO.Map                       as M
import           Control.Monad.State
import           Control.Lens
import           Data.Tuple                     ( swap )

import           Lang.Types

type RunEnv a = Map Text (RuntimeRes a)
type RunCoda m a = StateT (RunEnv a) m (RuntimeRes a)

data RuntimeRes a = RuntimeString {fromRuntimeRes :: a} 
    | RuntimeBundle {fromRuntimeRes :: a}
    deriving (Show, Read, Eq, Ord)

evalCoda :: (Exec m a, Ord a) => CodaVal -> m a
evalCoda cv = evalStateT (runCodaRes cv) mempty

runCodaRes :: (Exec m a, Ord a) => CodaVal -> StateT (RunEnv a) m a
runCodaRes cv = case cv of
    Dict d -> do
        evalRec <- mapM runCodaRes d
        lift (execRec evalRec)
    Let (Variable v) val body -> do
        res <- prepLetRhs v val
        at v ?= res
        runCodaRes body
    _ -> fromRuntimeRes <$> runCoda cv

runCoda :: (Exec m a, Ord a) => CodaVal -> RunCoda m a
runCoda cv = case cv of
    Var v -> lookupVar v
    Dir b p -> do
        bres <- runCodaRes b
        RuntimeBundle <$> lift (execDir bres p)
    Str t -> lift (RuntimeString <$> strLit t)
    rest -> error ("Impossible happened: not an RCO expr: " ++ show rest)

prepLetRhs :: (Exec m a, Ord a) => Text -> CodaVal -> RunCoda m a
prepLetRhs vn cv = case cv of
    Cl optEnv clcmd -> do
        opts <- (traverse . _2) runCodaRes optEnv
        let clinfo = ClInfo vn opts
        case clcmd of
            Run cmd -> processRun clinfo cmd
            ClCat val -> do
                valDep <- runCodaRes val
                lift (RuntimeString <$> clCat clinfo valDep)
            ClMake ks -> do
                valKs <- (traverse . _2) runCodaRes ks
                lift (RuntimeBundle <$> clMake clinfo valKs)
    Convert _ val TypeBundle -> do
        valRes <- runCoda val
        case valRes of
            RuntimeString t -> RuntimeBundle <$> lift (fromBundleName t)
            RuntimeBundle{} -> return valRes
    Dir{} -> runCoda cv
    Str{} -> runCoda cv
    Lit u -> lift (RuntimeBundle <$> clLit vn u)
    _     -> error "Impossible happened: not RCO expr in let assignment"

processRun :: (Exec m a) => ClInfo a -> [CMDEle CodaVal Text] -> RunCoda m a
processRun inf cmd = do
    prepCmd <- mapM prepCmdEle cmd
    let (depCmd, deps) = unzip prepCmd
    lift (RuntimeBundle <$> clRun inf (M.fromList (concat deps)) depCmd)
    where
        prepCmdEle :: (Exec m a) => CMDEle CodaVal Text -> StateT (RunEnv a) m (CodaCMDEle a, [(Text, a)])
        prepCmdEle ele = case ele of
            Plain t -> return (TextPlain t, [])
            CMDExpr e -> case e of
                Str t -> do
                    strRes <- lift (strLit t)
                    return (TextValue strRes, [])
                Var v -> do
                    vres <- lookupVar v
                    return $ case vres of
                        RuntimeString s -> (TextValue s, [])
                        RuntimeBundle b -> (BundleRef v, [(v, b)])
            _ -> error "Impossible happened: arguments in run"
        -- rmDupDep :: (Ord c, Ord a) => [CMDEle a b] -> [(a, c)] -> ([CMDEle c b], Map c a)
        -- rmDupDep txtCmd deps = (depToVar <$> txtCmd, varVal)
        --     where
        --         valVar = M.fromList deps
        --         depToVar ele = case ele of
        --             Plain s -> Plain s
        --             CMDExpr dep -> CMDExpr uniqVar
        --                 where uniqVar = maybe undefined id (M.lookup dep valVar)
        --         varVal = M.fromList (swap <$> M.toList valVar)

    
getPath :: [Text] -> CodaVal -> (Text, [Text])
getPath ps val = case val of
    Dir b sub -> getPath (sub : ps) b
    Var t     -> (t, ps)
    _         -> error "Impossible happened: expected variable in Dir"
lookupVar :: (Exec m a) => Text -> RunCoda m a
lookupVar v = fromMaybe errMsg <$> use (at v)
    -- impossible happened
    where errMsg = error ("Undefined variable: " ++ T.unpack v)
-- dirRootLookup :: (Exec m a) => Text -> StateT (RunEnv a) m (a, [Text])
-- dirRootLookup var = do
--     res <- lookupVar var
--     case res of
--         RuntimeBundle v vpath -> return (v, vpath)
--         _ -> error "Impossible happened: expected Bundle in Dir"
