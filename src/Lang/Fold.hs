{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Lang.Fold where

import           Lang.Types

import           RIO hiding (Lens, lens)
import           Control.Lens
import           RIO.State                      ( MonadState )
import           Control.Monad.State            ( StateT )



class (Monad m) => CodaLangEnv m a where
    lit :: UUID -> m a
    var :: VarName -> m a
    cl :: Cmd a -> m a
    dir :: a -> Text -> m a
    clet :: VarName -> a -> m a -> m a

-- data type
type VarMap a = Map VarName a

data LangRecord a = LangEnv {_counter :: Int, _env :: VarMap a}

makeLenses ''LangRecord

initEnv :: LangRecord a
initEnv = LangEnv 0 mempty

class HasCounter a where
    counterL :: Lens a a Int Int
instance HasCounter Int where
    counterL = id
instance HasCounter (LangRecord a) where
    counterL = counter

class HasEnv a b | a -> b where
    envL :: Lens a a (VarMap b) (VarMap b)
instance HasEnv (LangRecord a) a where
    envL = env

class (MonadState s m, HasCounter s) => GetCounter s m where
    getCounter :: m Int
    getCounter = do
        counterL += 1
        use counterL

instance (Monad m) => GetCounter (LangRecord a) (StateT (LangRecord a) m)

class (MonadState s m, HasEnv s b) => LocalVar s m b where
    withVar :: VarName -> b -> m a -> m a
    withVar varn val app = do
        oldEnv <- use envL
        (envL . at varn) ?= val
        res <- app
        envL .= oldEnv
        return res

foldCoda :: (CodaLangEnv m a, LocalVar s m b) => CodaVal -> m a
foldCoda (Lit u  ) = lit u
foldCoda (Var v  ) = var v
foldCoda (Cl  cmd) = do
    cmdval <- (bashcmd . traverse . cmdEleVal) foldCoda cmd
    cl cmdval
foldCoda (Dir bndl sub) = do
    root <- foldCoda bndl
    dir root sub
foldCoda (Let varname val body) = do
    val' <- foldCoda val
    clet varname val' (foldCoda body)