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
    -- bundle := var | dirRCO
    -- record := {text : value}
    -- str := string | var
    -- value := bundle | str | var
    -- cmdRCO := Cmd (var | string)
    -- dirRCO := Dir bundle text
    -- convert := Convert _ (value) TypeBundle
    -- letRCO := Let var (cmdRCO | dirRCO | lit | str | convert | record) rco
    -- rco := value | letRCO

import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import           RIO                     hiding ( to, Lens', lens )
import           Control.Lens
import qualified RIO.Text                      as T
import qualified RIO.Seq                       as S
import qualified RIO.Map                       as M
import           RIO.List                       ( foldl )

import           Lang.Types
import           Lang.Fold


type Bundle = CodaVal
type Value = CodaVal
type CmdRCO = Cmd RCOVal

-- cmdRCO | dirRCO | Lit id
type LetRhs = CodaVal

type NameBinding = (Text, LetRhs)
type NameBindings = S.Seq NameBinding
type Path = S.Seq Text
type RCOValMap = TextMap RCOVal
data Closure = Closure {clovarenv :: RCOValMap, clooptenv :: RCOValMap}
    deriving (Show)
data RCOVal = RCOLit UUID 
    | RCOBundle VarName
    | RCOVar VarName 
    | RCOCmd [(Text, RCOVal)] CmdRCO
    | RCODir VarName Path 
    | RCOStr Text
    | RCORec RCOValMap
    | RCOFun TypeDict Closure CodaVal
    deriving (Show)
data RCO a = RCO NameBindings a
    deriving (Show, Eq, Read, Functor)
instance (Applicative RCO) where
    pure = RCO mempty
    liftA2 f (RCO n1 a1) (RCO n2 a2) = RCO (n1 <> n2) (f a1 a2)
instance (Monad RCO) where
    (RCO n1 a1) >>= f = case f a1 of
        RCO n2 a2 -> RCO (n1 <> n2) a2

type RCORes = RCOVal

-- state data type
type RCOEnvVal = RCOVal
data RCOState = RCOState {
    _counter :: Int
    , _env :: VarMap RCOEnvVal
    , _context :: S.Seq Text
    , _optvar :: RCOValMap
    , _newvarenv :: RCOValMap
    }
makeLenses ''RCOState

closureL :: Lens' RCOState Closure
closureL = lens (\s -> (Closure (_env s) (_optvar s))) (\s (Closure varenv optenv) -> s{_env = varenv, _optvar = optenv} )

instance HasCounter RCOState where
    counterL = counter

instance HasEnv RCOState RCOEnvVal where
    envL = env

instance (Monad m) => GetCounter RCOState (StateT RCOState m)

instance LocalVar RCOState RCOPass RCOVal

withClosure :: Closure -> RCOPass a -> RCOPass a
withClosure clo app = do
    oldclo <- use closureL
    closureL .= clo
    res <- app
    closureL .= oldclo
    return res

withLocal :: Text -> RCOPass a -> RCOPass a
withLocal v app = do
    context %= (S.|> v)
    val <- app
    context %= (\(most S.:|> _) -> most)
    return val

withOptVar :: Text -> RCOVal -> RCOPass a -> RCOPass a
withOptVar opt varres app = do
    oldOpt <- use optvar
    (optvar . at opt) ?= varres
    res <- app
    optvar .= oldOpt
    return res
-- lang rco
runRCO :: CodaVal -> CodaVal
runRCO cdvl = foldr (\(k,v) -> (Let (Variable k) v)) bndl binds
  where
    foldApp :: RCOPass Bundle
    foldApp        = foldCoda cdvl >>= rcoValue >>= toRetVal
    RCO binds bndl = evalStateT foldApp (RCOState 0 mempty mempty mempty mempty)

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

bindNewName :: RCOVal -> RCOPass VarName
bindNewName cv = do
    name <- newTmpName
    newvarenv . at name ?= cv
    return name

bindName :: RCOVal -> RCOPass VarName
bindName cv = case cv of
    RCOVar v -> return v
    _ -> bindNewName cv

fromRCODir :: VarName -> Path -> Bundle
fromRCODir n path = foldl Dir (Var n) path

evalRCO :: RCOVal -> RCOPass VarName
evalRCO rcoval = case rcoval of
    RCOVar varn -> return varn
    RCOLit{} -> register
    RCOStr{} -> register
    RCOCmd{} -> register
    RCODir{} -> register
    _ -> error (show rcoval)
    where
        register = bindName rcoval


toRetVal :: RCOVal -> RCOPass Value
toRetVal (RCORec dt) = Dict <$> mapM toRetVal dt
toRetVal (RCOFun ad clo@(Closure varenv _) body) = do
    let
        newvarenv = M.union (M.mapWithKey (\k _ -> RCOVar k) ad) varenv
        newclo = clo{clovarenv = newvarenv}
    withClosure newclo $ do 
        bdv <- foldCoda body >>= toRetVal
        return (Lambda ad bdv) 
toRetVal (RCODir bname subdirs) = do
    _ <- toRetVal (RCOVar bname)
    return $ case subdirs of
        S.Empty -> Var bname
        _       -> foldl Dir (Var bname) subdirs
toRetVal (RCOStr t) = return (Str t)
toRetVal (RCOLit uuid) = return (Lit uuid)
toRetVal (RCOBundle v) = do
    _ <- toRetVal (RCOVar v)
    return (Convert Nothing (Var v) TypeBundle)
toRetVal (RCOVar varn) = do
    val <- use (newvarenv . at varn)
    let load v = do
            newvarenv . at varn .= Nothing
            codaval <- toRetVal v
            lift (RCO (S.singleton (varn, codaval)) (Var varn))
    maybe (return (Var varn)) load val
toRetVal (RCOCmd optEnv cmd) = do
    cmdVal <- mapM toRetVal cmd
    codaEnv <- (traverse . _2) toRetVal optEnv
    return (Cl codaEnv cmdVal)

rcoValue :: RCOVal -> RCOPass RCOVal
rcoValue v = case v of
    RCOCmd{} -> doEval
    RCOLit{} -> doEval
    RCODir{} -> noChange
    RCORec{} -> noChange
    RCOStr{} -> noChange
    RCOVar{} -> noChange
    RCOFun{} -> noChange
    RCOBundle{} -> error "illegal value" 
    where
        doEval = RCOVar <$> evalRCO v
        noChange = return v

rcoLet :: RCOVal -> RCOPass RCOVal
rcoLet v = case v of
    RCODir{} -> RCOVar <$> evalRCO v
    _ -> rcoValue v

toSubDir :: RCOVal -> Text -> RCOPass RCOVal
toSubDir arg p = case arg of
    RCODir bname subdirs -> return (RCODir bname (subdirs S.|> p))
    RCORec td -> return (fromMaybe undefined (M.lookup p td))
    RCOLit{} -> newSub
    RCOVar{} -> newSub
    RCOCmd{} -> newSub
    _ -> error "Illegal dir"
    where
        newSub = do
            newn <- evalRCO arg
            return (RCODir newn (S.singleton p))

instance CodaLangEnv RCOPass RCORes where
    lit uuid = return (RCOLit uuid)
    str t = return (RCOStr t)
    var v = use (envL . at v . to (fromMaybe errmsg))
      where
        errmsg =
            error
                (  "The impossible happened: undefined variable in RCO: "
                ++ T.unpack v
                )
    cl _ (Run cmd) = do
        optEnv <- use (optvar . to M.toList)
        RCOCmd optEnv <$> rcocmd
        where rcocmd = Run <$> traverse (>>= rcoLet) cmd
    dir val subdir = toSubDir val subdir
    clet af val body = case af of
        Variable vname -> do
            newns <- withLocal vname (val >>= rcoLet)
            withVar vname newns body
        OptionVar vname -> do
            optval <- val >>= rcoValue
            withOptVar vname optval body
    convert typeTag val t = do
        optEnv <- use (optvar . to M.toList)
        let catCmd v = RCOCmd optEnv (ClCat v)
            valType = fromMaybe (error "no type tag in RCO convert") typeTag
            makeVar c = Var <$> (bindName c)
            mapWithKeyM f m = sequence (M.mapWithKey f m)
            castFromTo :: RCOVal -> CodaType -> CodaType -> RCOPass RCOVal
            castFromTo fval fty tty 
                | fty `isSubtypeOf` tty = return fval
                | otherwise = case fty of
                    TypeBundle -> case tty of
                        TypeString -> catCmd <$> rcoValue fval
                        TypeRecord dt ->
                            RCORec <$> 
                                mapWithKeyM (\ k t -> (dir fval k) >>= ( `castBundle` t) >>= rcoValue) dt
                        TypeBundle -> return fval
                    TypeRecord fdict -> case tty of
                        TypeBundle -> do
                            let castKeyT k t = do
                                    dirres <- dir fval k
                                    castFromTo dirres t TypeBundle >>= rcoValue
                            bdRec <- mapWithKeyM castKeyT fdict
                            return (RCOCmd optEnv (ClMake (M.toList bdRec)))
                        TypeRecord tdict -> do
                            let convRecEle :: Text -> CodaType -> CodaType -> RCOPass RCOVal
                                convRecEle k ft tt = do
                                    dirres <- dir fval k
                                    castFromTo dirres ft tt >>= rcoValue
                            RCORec <$> sequence (M.intersectionWithKey convRecEle fdict tdict)
                        TypeString -> error "RCO: convert record to string"           
                    TypeString -> case tty of
                        TypeBundle -> do
                            fcoda <- rcoValue fval >>= bindName
                            RCOVar <$> bindName (RCOBundle fcoda)
                        TypeString -> return fval
                        TypeRecord{} -> error "RCO: convert string to record"
            castBundle bd ty = case ty of
                TypeBundle -> return bd
                _ -> castFromTo bd TypeBundle ty
        castFromTo val valType t

    dict rs = do
        rsv <- sequence rs
        RCORec <$> mapM rcoLet rsv
    
    lambda arg body = do
        varenv <- use envL
        optenv <- use optvar
        return (RCOFun arg (Closure varenv optenv) body)
    
    apply f argval = case f of
        RCOFun argdict clo@(Closure varenv _) body -> do
            let 
                newvarenv = M.union (M.intersection argval argdict) varenv
                newclo = clo {clovarenv = newvarenv}
            withClosure newclo (foldCoda body)
        _ -> error "applying non function"