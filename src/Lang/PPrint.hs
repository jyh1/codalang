{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lang.PPrint(codaToDoc, pprintCodaWidth, pprintCoda) where

import Lang.Types
import Lang.Fold

import RIO hiding (to)
import qualified RIO.Text as T
import RIO.List (zipWith, repeat)
import Control.Monad.State
import Control.Lens
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

data Anno = DirAnno | Keyword | VarAnno Text Int | LitAnno | StrAnno | RunAnno | LetAnno | StmtAnno
    deriving (Show, Read, Eq, Ord)

data PPState = PPState {_counter :: Int, _env :: VarMap Int}
    deriving (Show)
makeLenses ''PPState 

type AnnoDoc = Doc Anno

data PPrint = Value AnnoDoc | PLet [AnnoDoc] AnnoDoc
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
toAnnoDoc (PLet as body) = annotate LetAnno (hang 3 (sep [defs, body]))
    where
        keyword = annotate Keyword
        keylet = keyword "let"
        keyin = keyword "in"
        asdoc = sep (punctuate semi as)
        defs = align (sep [hang 4 (sep [keylet, asdoc]), keyin])

toAnnoDocWithParen :: PPrint -> AnnoDoc
toAnnoDocWithParen pval = case pval of
    PLet{} -> parens (toAnnoDoc pval)
    _ -> toAnnoDoc pval

instance CodaLangEnv PPPass PPrint where
    lit u = ranno LitAnno (pretty (show u))
    -- var :: VarName -> m a
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
    dir bval sub = 
        ranno DirAnno (toAnnoDocWithParen bval <> "/" <> pretty sub)
    clet vn val body = do
        c <- getCounter
        valdoc <- toAnnoDoc <$> val
        let stmt = annotate StmtAnno (hsep [pretty vn, "=", valdoc])
        bodydoc <- withVar vn c body
        return $ case bodydoc of
            Value d -> PLet [stmt] d
            PLet ss d -> PLet (stmt : ss) d

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