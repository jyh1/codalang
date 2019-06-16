
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- eliminate record recurrence after RCO
-- ret = bundle | str | record ({Text: ret})
-- res = ret | Let var rhs ret

module Lang.EliminateRecord (runER) where

import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import           RIO                     hiding ( to, at, view )
import           Control.Lens
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T

import           Lang.Types
import           Lang.Fold

type EREnvVal = CodaVal
type ERState = TextMap EREnvVal

type ERPass = StateT ERState Identity
instance LocalVar ERState ERPass CodaVal

instance CodaLangEnv ERPass CodaVal where
    lit uuid = return (Lit uuid)
    str t = return (Str t)
    var v = use (envL . at v . to (maybe (Var v) id))
    cl cmd = Cl <$> (sequence cmd)
    dir r p = return $ case r of
        Dict d -> view (at p . to (fromMaybe err)) d
        _ -> Dir r p
        where
            err = error ("Key error during EliminateRecord: " ++ T.unpack p)
    clet (Variable var) res val = do
        resVal <- res
        case resVal of
            Dict{} -> rmVar resVal
            Var{} -> rmVar resVal
            _ -> Let (Variable var) resVal <$> val
        where
            rmVar resVal = (envL %= M.insert var resVal) >> val
    convert fty val tty = case tty of
        TypeBundle -> case val of
            Dict d -> return (Cl (ClMake (M.toList d)))
            _ -> return (Convert fty val tty)
        _ -> error "Unable to make new bundle"
    -- dict :: Map Text (m a) -> m a
    dict d = Dict <$> (sequence d)

runER :: CodaVal -> CodaVal
runER cdvl = newAST
  where
    foldApp :: ERPass CodaVal
    foldApp = foldCoda cdvl
    newAST = runIdentity (evalStateT foldApp mempty)