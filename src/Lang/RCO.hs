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
    -- cmdRCO := Cmd value
    -- dirRCO := Dir bundle text
    -- convert := Convert _ (value) TypeBundle
    -- letRCO := Let var (cmdRCO | dirRCO | lit | str | convert | record) rco
    -- rco := value | letRCO

import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                )
import           RIO                     hiding ( to )
import           Control.Lens
import qualified RIO.Text                      as T
import qualified RIO.Seq                       as S
import qualified RIO.Map                       as M
import           RIO.List                       ( foldl )

import           Lang.Types
import           Lang.Fold

-- state data type
type RCOEnvVal = VarName
data RCOState = RCOState {_counter :: Int, _env :: VarMap RCOEnvVal, _context :: S.Seq Text, _optvar :: VarMap CodaVal}
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

withOptVar :: Text -> CodaVal -> RCOPass a -> RCOPass a
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
    foldApp        = foldCoda cdvl >>= toValue
    RCO binds bndl = evalStateT foldApp (RCOState 0 mempty mempty mempty)


type Bundle = CodaVal
type Value = CodaVal
type CmdRCO = Cmd Bundle

-- cmdRCO | dirRCO | Lit id
type LetRhs = CodaVal

type NameBinding = (Text, LetRhs)
type NameBindings = S.Seq NameBinding
type Path = S.Seq Text
data RCOVal = RCOLit UUID 
    | RCOVar VarName 
    | RCOCmd OptEnv CmdRCO 
    | RCODir VarName Path 
    | RCOStr Text
    | RCORec (TextMap Value)
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

bindNewName :: CodaVal -> RCOPass VarName
bindNewName cv = do
    name <- newTmpName
    lift (RCO (S.singleton (name, cv)) name)

bindName :: LetRhs -> RCOPass VarName
bindName cv = case cv of
    Var v -> return v
    _ -> bindNewName cv

fromRCODir :: VarName -> Path -> Bundle
fromRCODir n path = foldl Dir (Var n) path

toVarName :: RCOVal -> RCOPass VarName
toVarName (RCOLit uuid         ) = bindName (Lit uuid)
toVarName (RCOStr t) = bindName (Str t)
toVarName (RCOVar varn         ) = return varn
toVarName (RCOCmd optEnv cmd   ) = bindName (Cl optEnv cmd)
toVarName (RCODir bname subdirs) = bindName (fromRCODir bname subdirs)
toVarName (RCORec m) = bindName (Dict m)

toValue :: RCOVal -> RCOPass Value
toValue (RCODir bname subdirs) = return $ case subdirs of
    S.Empty -> Var bname
    _       -> foldl Dir (Var bname) subdirs
toValue (RCOStr t) = return (Str t)
toValue rest = Var <$> toVarName rest


-- (lit | dir | var)
-- toLitValue :: RCOVal -> RCOPass Value
-- toLitValue (RCOLit u) = return (Lit u)
-- toLitValue val = toValue val

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
    cl _ (Run cmd) = do
        optEnv <- use (optvar . to M.toList)
        RCOCmd optEnv <$> rcocmd
        where rcocmd = Run <$> traverse (>>= toValue) cmd
    dir val subdir = do
        (newn, path) <- toSubDir val
        return (RCODir newn (path S.|> subdir))
    -- clet (Variable vname) val body = do
    clet af val body = case af of
        Variable vname -> do
            newns <- withLocal vname (val >>= toVarName)
            withVar vname newns body
        OptionVar vname -> do
            optval <- val >>= toValue
            withOptVar vname optval body
    convert typeTag val t = do
        optEnv <- use (optvar . to M.toList)
        cval <- toValue val
        let catCmd v = return (RCOCmd optEnv (ClCat v))
            valType = fromMaybe (error "no type tag in RCO convert") typeTag
            makeVar c = Var <$> (bindName c)
            mapWithKeyM f m = sequence (M.mapWithKey f m)
            castFromTo :: Value -> CodaType -> CodaType -> RCOPass Value
            castFromTo fval fty tty 
                | fty `isSubtypeOf` tty = return fval
                | otherwise = case fty of
                    TypeBundle -> case tty of
                        TypeString -> catCmd fval >>= toValue
                        TypeRecord dt -> do
                            rcd <- Dict <$> 
                                mapWithKeyM (\ k t -> castBundle (Dir fval k) t) dt
                            makeVar rcd
                        TypeBundle -> return fval
                    TypeRecord fdict -> case tty of
                        TypeBundle -> do
                            bdRec <-  
                                mapWithKeyM (\k t -> castFromTo (Dir fval k) t TypeBundle) fdict
                            makeCmd <- makeVar (Cl optEnv (ClMake (M.toList bdRec)))
                            makeVar (Convert Nothing makeCmd TypeBundle)
                        TypeRecord tdict -> do
                            let convRecEle :: Text -> CodaType -> CodaType -> RCOPass Value
                                convRecEle k ft tt = 
                                    castFromTo (Dir fval k) ft tt
                            proRec <- Dict <$> sequence (M.intersectionWithKey convRecEle fdict tdict)
                            makeVar proRec
                        TypeString -> error "RCO: convert record to string"           
                    TypeString -> case tty of
                        TypeBundle -> makeVar (Convert (Just fty) fval tty)
                        TypeString -> return fval
                        TypeRecord{} -> error "RCO: convert string to record"
            castBundle bd ty = case ty of
                TypeBundle -> return bd
                _ -> castFromTo bd TypeBundle ty
        RCOVar <$> (castFromTo cval valType t >>= bindName)



    dict rs =
        RCORec <$> traverse (>>= toValue) rs
