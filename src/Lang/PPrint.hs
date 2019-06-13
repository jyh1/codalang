{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.PPrint(codaToDoc, pprintCodaWidth, pprintCoda, pprintCompact) where

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

data PPrint = Value AnnoDoc | PLet [AnnoDoc] AnnoDoc | PTypeAnno AnnoDoc
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
toAnnoDoc (PLet as body) = annotate LetAnno (hang 3 (sep [defs, body]))
    where
        keyword = annotate Keyword
        keylet = keyword "let"
        keyin = keyword "in"
        asdoc = sep (punctuate semi as)
        defs = align (sep [hang 4 (sep [keylet, asdoc]), keyin])

toAnnoDocWithParen :: PPrint -> AnnoDoc
toAnnoDocWithParen pval = case pval of
    Value{} -> toAnnoDoc pval
    _ -> parens (toAnnoDoc pval)

dictAnno :: [(Doc ann, Doc ann)] -> Doc ann
dictAnno ads = group (encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " dicLis)
    where 
        dicLis = [ sep [cat [k, ":"], v] | (k, v) <- ads]

instance (Pretty CodaType) where
    pretty TypeString = "String"
    pretty TypeBundle = "{_}"
    pretty (TypeRecord dict) = dictAnno [ (pretty k, pretty v) | (k, v) <- M.toList dict]

instance CodaLangEnv PPPass PPrint where
    lit u = case u of
        UUID{} -> ranno LitAnno (pretty (show u))
        BundleName n -> foldCoda (Convert (Str n) TypeBundle)
    var vn = do
        c <- use (envL . at vn . to (fromMaybe errmsg))
        ranno (VarAnno vn c) (pretty vn)
        where
            errmsg =
                error ("Undefined variable in PPrint: " ++ T.unpack vn)
    str s = ranno StrAnno (pretty (show s))
    cl (Run cmd) = do
        cs <- sequence cmd
        let cs' = toAnnoDoc <$> cs
            lpr = flatAlt "( " "("
            rpr = (flatAlt " )" ")")
            s = ", "
            runeles = case cs' of
                [] -> error "PPrint: empty run command"
                [e] -> lpr <> e <> comma <> rpr
                _ -> cat (zipWith (<>) (lpr : repeat s) cs') <> rpr
        ranno RunAnno (group runeles)
    cl (ClCat val) = val >>= (`convert` TypeString)
    dir bval sub = 
        ranno DirAnno (toAnnoDocWithParen bval <> "/" <> pretty sub)
    clet vn val body = do
        c <- getCounter
        valdoc <- toAnnoDoc <$> val
        let stmt = annotate StmtAnno (hsep [pretty vn, "=", valdoc])
        bodydoc <- withVar vn c body
        return $ case bodydoc of
            Value d -> PLet [stmt] d
            PTypeAnno d -> PLet [stmt] d
            PLet ss d -> PLet (stmt : ss) d
    convert val ct = return (PTypeAnno (annotate TypeAnno (fillCat [toAnnoDocWithParen val, " :: ", pretty ct])))
    dict d = do
        dres <- sequence d
        return (Value (dictAnno [ (pretty k, toAnnoDoc v) | (k, v) <- M.toList dres]))

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

pprintCompact :: CodaVal -> Text
pprintCompact cv = renderStrict (layoutCompact (codaToDoc cv))