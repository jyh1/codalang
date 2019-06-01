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

import           Lang.Types

type RunEnv a = Map Text (RuntimeRes a)
type RunCoda m a = StateT (RunEnv a) m (RuntimeRes a)

evalCoda :: (Exec m a) => CodaVal -> m (RuntimeRes a)
evalCoda cv = evalStateT (runCoda cv) mempty

runCoda :: (Exec m a) => CodaVal -> RunCoda m a
runCoda cv = case cv of
    Var v -> lookupVar v
    Dir{} -> do
        let (bundleVar, path) = getPath [] cv
        (v, vpath) <- dirRootLookup bundleVar
        return (RuntimeBundle v (vpath ++ path))
    Str t          -> return (RuntimeString t)
    Let v val body -> do
        res <- prepLetRhs v val
        at v ?= res
        runCoda body
    _ -> error "Impossible happened: not an RCO expr"

prepLetRhs :: (Exec m a) => Text -> CodaVal -> RunCoda m a
prepLetRhs vn cv = case cv of
    Cl (Run cmd) -> do
        prepCmd <- mapM prepCmdEle cmd
        let (txtCmd, deps) = unzip prepCmd
            depRep         = M.fromList (concat deps)
        lift (emptyBundle <$> clRun vn depRep txtCmd)
      where
        prepCmdEle ele = case ele of
            Str t -> return (t, [])
            Var v -> do
                vres <- lookupVar v
                return $ case vres of
                    RuntimeString s    -> (s, [])
                    RuntimeBundle b ps -> (v, [(v, Deps b ps)])
            Dir{} -> do
                (v, vpath) <- dirRootLookup dirRoot
                return (buildPath (dirRoot : path), [(dirRoot, Deps v vpath)])
                where (dirRoot, path) = getPath [] ele
            _ -> error "Impossible happened: arguments in run"
    Dir{} -> runCoda cv
    Str{} -> runCoda cv
    Lit u -> lift (emptyBundle <$> clLit vn u)
    _     -> error "Impossible happened: not RCO expr in let assignment"


getPath :: [Text] -> CodaVal -> (Text, [Text])
getPath ps val = case val of
    Dir b sub -> getPath (sub : ps) b
    Var t     -> (t, ps)
    _         -> error "Impossible happened: expected variable in Dir"
lookupVar :: (Exec m a) => Text -> RunCoda m a
lookupVar v = fromMaybe errMsg <$> use (at v)
    -- impossible happened
    where errMsg = error ("Undefined variable: " ++ T.unpack v)
dirRootLookup :: (Exec m a) => Text -> StateT (RunEnv a) m (a, [Text])
dirRootLookup var = do
    res <- lookupVar var
    case res of
        RuntimeBundle v vpath -> return (v, vpath)
        _ -> error "Impossible happened: expected Bundle in Dir"
emptyBundle :: a -> RuntimeRes a
emptyBundle v = RuntimeBundle v []
