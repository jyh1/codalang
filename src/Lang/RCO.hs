{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.RCO
    ( runRCO
    )
where
-- remove complex operations
    -- bundle := Var var | dirRCO
    -- str := string | var
    -- value := bundle | str
    -- cmdRCO := Cmd value
    -- dirRCO := Dir (bundle | lit) text
    -- letRCO := Let var (cmdRCO | dirRCO | lit | str) rco
    -- rco := value | letRCO

import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import           RIO                     hiding ( to )
import           Control.Lens
import qualified RIO.Text                      as T
import qualified RIO.Seq                       as S
import           RIO.List                       ( foldl )

import           Lang.Types
import           Lang.Fold

-- state data type
type RCOEnvVal = VarName
data RCOState = RCOState {_counter :: Int, _env :: VarMap RCOEnvVal, _context :: S.Seq Text}
makeLenses ''RCOState


instance HasCounter RCOState where
    counterL = counter

instance HasEnv RCOState RCOEnvVal where
    envL = env

instance (Monad m) => GetCounter RCOState (StateT RCOState m)

instance LocalVar RCOState RCOPass Text

withLocal :: Text -> RCOPass a -> RCOPass a
withLocal v app = do
    context %= (S.|> v)
    val <- app
    context %= (\(most S.:|> _) -> most)
    return val
-- lang rco
runRCO :: CodaVal -> CodaVal
runRCO cdvl = foldr (uncurry Let) bndl binds
  where
    foldApp :: RCOPass Bundle
    foldApp        = foldCoda cdvl >>= toValue
    RCO binds bndl = evalStateT foldApp (RCOState 0 mempty mempty)


type Bundle = CodaVal
type Value = CodaVal
type CmdRCO = Cmd Bundle

-- cmdRCO | dirRCO | Lit id
type LetRhs = CodaVal

type NameBinding = (Text, LetRhs)
type NameBindings = S.Seq NameBinding
type Path = S.Seq Text
data RCOVal = RCOLit UUID | RCOVar VarName | RCOCmd CmdRCO | RCODir VarName Path | RCOStr Text
data RCO a = RCO NameBindings a
    deriving (Show, Eq, Read, Functor)
instance (Applicative RCO) where
    pure = RCO mempty
    liftA2 f (RCO n1 a1) (RCO n2 a2) = RCO (n1 S.>< n2) (f a1 a2)
instance (Monad RCO) where
    (RCO n1 a1) >>= f = case f a1 of
        RCO n2 a2 -> RCO (n1 S.>< n2) a2

type RCORes = RCOVal

type RCOPass = StateT RCOState RCO

-- utility functions

newTmpName :: RCOPass VarName
newTmpName = do
    suff <- getCounter
    stems <- use (context . to getName)
    return (T.intercalate "-" (stems ++ [tshow suff]))
  where
    getName S.Empty = [tmpName]
    getName cs      = toList cs

bindName :: LetRhs -> RCOPass VarName
bindName cv = do
    name <- newTmpName
    lift (RCO (S.singleton (name, cv)) name)

fromRCODir :: VarName -> Path -> Bundle
fromRCODir n path = foldl Dir (Var n) path

toVarName :: RCOVal -> RCOPass VarName
toVarName (RCOLit uuid         ) = bindName (Lit uuid)
toVarName (RCOStr t) = bindName (Str t)
toVarName (RCOVar varn         ) = return varn
toVarName (RCOCmd cmd          ) = bindName (Cl cmd)
toVarName (RCODir bname subdirs) = bindName (fromRCODir bname subdirs)

toValue :: RCOVal -> RCOPass Value
toValue (RCODir bname subdirs) = return $ case subdirs of
    S.Empty -> Var bname
    _       -> foldl Dir (Var bname) subdirs
toValue (RCOStr t) = return (Str t)
toValue rest = Var <$> toVarName rest

toSubDir :: RCOVal -> RCOPass (VarName, Path)
toSubDir (RCODir bname subdirs) = return (bname, subdirs)
toSubDir rest                   = do
    newn <- toVarName rest
    return (newn, Empty)

instance CodaLangEnv RCOPass RCORes where
    lit uuid = return (RCOLit uuid)
    str t = return (RCOStr t)
    var v = use (envL . at v . to (RCOVar . fromMaybe errmsg))
      where
        errmsg =
            error
                (  "The impossible happened: undefined variable in RCO: "
                ++ T.unpack v
                )
    cl (Run cmd) = RCOCmd <$> rcocmd
        where rcocmd = Run <$> traverse (>>= toValue) cmd
    dir val subdir = do
        (newn, path) <- toSubDir val
        return (RCODir newn (path S.|> subdir))
    clet vname val body = do
        newns <- withLocal vname (val >>= toVarName)
        withVar vname newns body



