{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.PPrint(codaToDoc
    , pprintCodaWidth
    , pprintCoda
    , pprintCompact
    , pprintType) 
    where

import Lang.Types
import Lang.Fold

import RIO hiding (to)
import qualified RIO.Text as T
import qualified RIO.Map as M
import RIO.List (zipWith, repeat)
import Control.Monad.State
import Control.Lens
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

data Anno = DirAnno | Keyword | VarAnno Text Int | LitAnno | StrAnno | RunAnno | LetAnno | StmtAnno | TypeAnno
    deriving (Show, Read, Eq, Ord)

data PPState = PPState {_counter :: Int, _env :: VarMap Int}
    deriving (Show)
makeLenses ''PPState 

type AnnoDoc = Doc Anno

data PPrint = Value AnnoDoc 
    | PLet [AnnoDoc] AnnoDoc 
    | PTypeAnno AnnoDoc 
    | PApply AnnoDoc AnnoDoc
    | PLambda AnnoDoc AnnoDoc
    deriving (Show)

type PPPass = StateT PPState Identity
instance HasCounter PPState where
    counterL = counter
instance GetCounter PPState PPPass
instance HasEnv PPState Int where
    envL = env

instance LocalVar PPState PPPass Int

anno :: Anno -> AnnoDoc -> PPrint
anno ann doc = Value (annotate ann doc)

ranno :: (Monad m) => Anno -> AnnoDoc -> m PPrint
ranno a d = return (anno a d)

toAnnoDoc :: PPrint -> AnnoDoc
toAnnoDoc (Value d) = d
toAnnoDoc (PTypeAnno d) = d
toAnnoDoc (PLet as body) = annotate LetAnno (hang 4 (sep [defs, body]))
    where
        keyword = annotate Keyword
        keylet = keyword "let"
        keyin = keyword "in"
        asdoc = sep (punctuate semi as)
        defs = align (sep [hang 4 (sep [keylet, asdoc]), keyin])
toAnnoDoc (PApply f a) = f <> a
toAnnoDoc (PLambda args body) = args <+> "=>" <+> body

toAnnoDocWithParen :: PPrint -> AnnoDoc
toAnnoDocWithParen pval = case pval of
    Value{} -> toAnnoDoc pval
    _ -> parens (toAnnoDoc pval)

objectAnno :: Doc ann -> Doc ann -> [(Doc ann, Doc ann)] -> Doc ann
objectAnno op cl ads = group (align $ encloseSep (flatAlt (op <> line <> "  ") op) (flatAlt (line <> cl) cl) ", " dicLis)
    where 
        dicLis = [hang 4 (k <> ":" <+> v) | (k, v) <- ads]

dictAnno, argAnno :: [(Doc ann, Doc ann)] -> Doc ann
dictAnno = objectAnno "{" "}"
argAnno = objectAnno "[" "]"

textMap :: TextMap (Doc ann) -> [(Doc ann, Doc ann)]
textMap m =  [(pretty k, v) | (k, v) <- M.toList m]

instance (Pretty CodaType) where
    pretty TypeString = "string"
    pretty TypeBundle = "bundle"
    pretty (TypeRecord dict) = dictAnno (textMap (pretty <$> dict))
    pretty (TypeLam f a) = 
        argAnno (textMap (pretty <$> f)) <+> "=>" <+> pretty a

instance CodaLangEnv PPPass PPrint where
    lit u = case u of
        UUID{} -> ranno LitAnno (pretty (show u))
        BundleName n -> foldCoda (defConvert (Str n) TypeBundle)
    var vn = do
        c <- use (envL . at vn . to (fromMaybe errmsg))
        ranno (VarAnno vn c) (pretty vn)
        where
            errmsg =
                error ("Undefined variable in PPrint: " ++ T.unpack vn)
    str s = ranno StrAnno (pretty (show s))
    cl optEnv cmd
        | not (null optEnv) = 
            foldr ($) (cl [] cmd) [ clet (OptionVar optVar) (return optVal) | (optVar, optVal) <- optEnv]
        | otherwise = do
            cmd' <- sequence cmd
            case cmd' of
                Run cs -> do
                    let 
                        cs' = fromCMDEle annoExpr annoPlain <$> cs
                        lpr = "@"
                        rpr = "@"
                        runeles = align $ (lpr <> mconcat cs' <> rpr)
                    ranno RunAnno (group runeles)
                -- ClCat val -> val >>= (\v -> convert Nothing v TypeString)
                -- ClMake ks -> dict (M.fromList ks) >>= (\v -> convert Nothing v TypeBundle)
    dir bval sub = 
        ranno DirAnno (toAnnoDocWithParen bval <> "/" <> pretty sub)
    clet as val body = do
        let vn = printAssignForm as
        c <- getCounter
        valdoc <- val
        let stmt = case valdoc of
                PLambda arg ret -> annotate StmtAnno (hsep [pretty vn <> arg, "=", ret])
                _ -> annotate StmtAnno (hsep [pretty vn, "=", toAnnoDoc valdoc])
        bodydoc <- withVar vn c body
        return $ case bodydoc of
            PLet ss d -> PLet (stmt : ss) d
            _ -> PLet [stmt] (toAnnoDoc bodydoc)
    convert _ val ct = return (PTypeAnno (annotate TypeAnno (align (toAnnoDocWithParen val <+> "as" <+> pretty ct))))
    dict d = do
        dres <- sequence d
        return (Value (dictAnno (textMap (toAnnoDoc <$> dres))))
    lambda ad body = PLambda (argAnno (textMap (pretty <$> ad))) <$> (toAnnoDoc <$> foldCoda body)
    apply f arg = 
        return (PApply (toAnnoDocWithParen f) (argAnno (textMap (toAnnoDoc <$> arg))))

annoPlain :: Text -> AnnoDoc
annoPlain t = pretty (concat (map escChar s))
    where
        s = T.unpack t
        escChar c = bool [c] (['\\', c]) (c == '\\' || c == '@' || c == '$')

annoExpr :: PPrint -> AnnoDoc
annoExpr d = "${" <> toAnnoDoc d <> "}"

codaToDoc :: CodaVal -> AnnoDoc
codaToDoc cv = toAnnoDoc res
    where
        app :: PPPass PPrint
        app = foldCoda cv
        res = runIdentity (evalStateT app (PPState 0 mempty))

pprintCodaWidth :: Int -> CodaVal -> Text
pprintCodaWidth n cv = renderStrict (layoutSmart opt doc)
    where
        doc = codaToDoc cv
        opt = LayoutOptions (AvailablePerLine n 1)

pprintCoda :: CodaVal -> Text
pprintCoda = pprintCodaWidth 75

pprintType :: CodaType -> Text
pprintType ty = renderStrict (layoutSmart opt (pretty ty))
    where
        opt = LayoutOptions (AvailablePerLine 75 1) 

pprintCompact :: CodaVal -> Text
pprintCompact cv = renderStrict (layoutCompact (codaToDoc cv))