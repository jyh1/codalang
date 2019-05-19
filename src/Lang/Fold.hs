{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Lang.Fold where

import           Lang.Types

import           RIO
import           Control.Lens                   ( makeLenses
                                                , (+=)
                                                , use
                                                )
import           RIO.State                      ( MonadState )
import           Control.Monad.State            ( StateT )



class (Monad m) => CodaLangEnv m a where
    lit :: UUID -> m a
    var :: VarName -> m a
    cl :: Cmd a -> m a
    dir :: a -> Text -> m a
    clet :: VarName -> a -> m a -> m a
    sandBox :: m a -> m a

foldCoda :: (CodaLangEnv m a) => CodaVal -> m a
foldCoda (Lit u  ) = lit u
foldCoda (Var v  ) = var v
foldCoda (Cl  cmd) = do
    cmdval <- (bashcmd . traverse) foldCmdEle cmd
    cl cmdval
  where
    foldCmdEle :: (CodaLangEnv m a) => CmdEle CodaVal -> m (CmdEle a)
    foldCmdEle (Val      v) = Val <$> (sandBox (foldCoda v))
    foldCmdEle (Verbatim t) = return (Verbatim t)
foldCoda (Dir bndl sub) = do
    root <- foldCoda bndl
    dir root sub
foldCoda (Let varname val body) = do
    val' <- sandBox (foldCoda val)
    clet varname val' (foldCoda body)

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
