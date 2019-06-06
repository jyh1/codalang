{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Lang.Fold where

import           Lang.Types

import           RIO                     hiding ( Lens
                                                , lens
                                                , over
                                                )
import           Control.Lens
import           RIO.State                      ( MonadState )



class (Monad m) => CodaLangEnv m a where
    lit :: UUID -> m a
    var :: VarName -> m a
    str :: Text -> m a
    cl :: Cmd (m a) -> m a
    dir :: a -> Text -> m a
    clet :: VarName -> m a -> m a -> m a
    convert :: a -> CodaType -> m a

-- data type
type VarMap a = Map VarName a


class HasCounter a where
    counterL :: Lens a a Int Int
instance HasCounter Int where
    counterL = id

class HasEnv a b | a -> b where
    envL :: Lens a a (VarMap b) (VarMap b)
instance HasEnv (VarMap a) a where
    envL = id

class (MonadState s m, HasCounter s) => GetCounter s m where
    getCounter :: m Int
    getCounter = do
        counterL += 1
        use counterL

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
foldCoda (Str s) = str s
foldCoda (Cl  cmd) = do
    let cmdval = fmap foldCoda cmd
    cl cmdval
foldCoda (Dir bndl sub) = do
    root <- foldCoda bndl
    dir root sub
foldCoda (Let varname val body) = 
    clet varname (foldCoda val) (foldCoda body)
foldCoda (Convert val rt) = foldCoda val >>= (`convert` rt)
