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
    cl :: [(Text, a)] -> Cmd (m a) -> m a
    dir :: a -> Text -> m a
    clet :: AssignForm -> m a -> m a -> m a
    convert :: (Maybe CodaType) -> a -> CodaType -> m a
    dict :: Map Text (m a) -> m a
    lambda :: TypeDict -> m a -> m a
    apply :: a -> a -> m a

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
    withVar varn val app = sandBox $ do
        (envL . at varn) ?= val
        app
    sandBox :: m a -> m a
    sandBox app = do
        oldEnv <- use envL
        res <- app
        envL .= oldEnv
        return res

foldCoda :: (CodaLangEnv m a, LocalVar s m b) => CodaVal -> m a
foldCoda (Lit u  ) = lit u
foldCoda (Var v  ) = var v
foldCoda (Str s) = str s
foldCoda (Cl opts cmd) = do
    let cmdval = fmap foldCoda cmd
    optEnv <- (traverse . _2) foldCoda opts
    cl optEnv cmdval
foldCoda (Dir bndl sub) = do
    root <- foldCoda bndl
    dir root sub
foldCoda (Let varname val body) = 
    clet varname (foldCoda val) (foldCoda body)
foldCoda (Convert domain val rt) = foldCoda val >>= (\v -> convert domain v rt)
foldCoda (Dict d) = dict (foldCoda <$> d)
foldCoda (Lambda ad bd) = lambda ad (foldCoda bd)
foldCoda (Apply f bd) = join (liftA2 apply (foldCoda f) (foldCoda bd))