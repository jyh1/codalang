{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | random print an CodaVal to be parsed back
module LangTest.RandPrint(randomPrintCoda) where

import Lang.Types

import RIO
import RIO.List
import qualified RIO.Text as T
import qualified RIO.Map as M
import Test.QuickCheck hiding (Result)
import Control.Lens (_1, _last)

data RendCoda = Parens RendCoda 
    | Parens1 RendCoda
    -- the outmost pair of parens without space
    | TightParens RendCoda
    -- must insert one pair of parens with no space
    | TightParens1 RendCoda
    | Spaces RendCoda
    | Space1 RendCoda
    | RLis [RendCoda]
    | RStr Text
    | Symbol Text
    | RLit UUID
    | RLet [RendCoda] RendCoda
    | ColonAnnot Text RendCoda RendCoda
    | RType CodaType
    | RDic (Map Text RendCoda)
    deriving (Show, Read, Eq, Ord)

-- | symbol that could be surrounded with spaces
spaceSymbol, space1Symbol :: Text -> RendCoda
spaceSymbol = Spaces . Symbol
space1Symbol = Space1 . spaceSymbol

entity :: RendCoda -> RendCoda
entity = Parens . Spaces

entSym :: Text -> RendCoda
entSym = entity . Symbol

singleColon = ColonAnnot ":"
doubleColon = ColonAnnot "::"

doRend :: RendCoda -> Gen String
doRend rc = case rc of
    RLit u -> return $ show u
    RStr s -> return $ show s
    RLis as -> concat <$> mapM doRend as
    Symbol s -> return (T.unpack s)
    Spaces r -> doRend r >>= encloseSpaces
    Space1 r -> doRend (RLis [Symbol " ", r, Symbol " "])
    Parens r -> do
        dep <- choose (0, 3)
        doRend (nTimes dep Parens1 r)
    Parens1 r -> doRend (RLis [spaceSymbol "(", r, spaceSymbol ")"])
    TightParens r -> do
        flag <- coin
        doRend $ bool (rmSpace r) (TightParens1 (Parens r)) flag
    TightParens1 r -> doRend (RLis [Symbol "(", r, Symbol ")"])
    RLet as body -> case as of
        [] -> doRend body
        _ -> do
            (picked, rest) <- splitLists as
            newAs <- insertSemi (spaceSymbol ";") picked
            doRend (RLis (concat [[space1Symbol "let"], newAs, [space1Symbol "in", RLet rest body]]))
    ColonAnnot t val anot -> doRend (RLis [val, spaceSymbol t, anot])
    RType ct -> case ct of
        TypeString -> doRend (spaceSymbol "String")
        TypeBundle -> doRend (enloseParen "{" "}" (spaceSymbol "_"))
        TypeRecord d -> doRend (RDic (RType <$> d))
    RDic dic -> doRend (enloseParen "{" "}" (RLis annotComma))
        where
            annotLis = [singleColon (spaceSymbol k) v | (k, v) <- M.toList dic]
            annotComma = intersperse (spaceSymbol ",") annotLis
    where
        nTimes :: Int -> (a -> a) -> (a -> a)
        nTimes 0 _ = id
        nTimes 1 f = f
        nTimes n f = f . nTimes (n-1) f
        rmSpace :: RendCoda -> RendCoda
        rmSpace (Parens r) = rmSpace r
        rmSpace (Spaces r) = rmSpace r
        rmSpace (Parens1 r) = TightParens1 (rmSpace r)
        rmSpace (RLis as) = case as of
            [] -> RLis []
            _ -> RLis (over _last rmSpace as)
        rmSpace ras@RLet{} = TightParens1 ras
        rmSpace ras@ColonAnnot{} = TightParens1 ras
        rmSpace rest = rest
        enloseParen ll lr cont = RLis [spaceSymbol ll, cont, spaceSymbol lr]


splitLists :: [a] -> Gen ([a], [a])
splitLists [] = return ([], [])
splitLists as = do
    k <- choose (1, len)
    return (splitAt k as)
    where
        len = length as

insertComma :: a -> [a] -> [a]
insertComma sep as = case as of
    [] -> error "Empty list"
    [a] -> [a, sep]
    _ -> intersperse sep as

insertSemi :: a -> [a] -> Gen [a]
insertSemi sep as = case as of
    [] -> error "Empty list"
    _ -> do
        let inserted = intersperse sep as
        bool (inserted ++ [sep]) inserted <$> coin
        
coin :: Gen Bool
coin = choose (False, True)
-- | random white characters
spaces :: Gen String
spaces = resize 2 (listOf space)
    where
        space = elements "\t \n"

encloseSpaces :: String -> Gen String
encloseSpaces s = liftA2 (\s1 s2 -> s1 ++ s ++ s2) spaces spaces

rendCoda :: CodaVal -> RendCoda
rendCoda cv = case cv of
    Lit u -> entity (RLit u)
    Var v -> entSym v
    Cl (Run rs) -> entity (Parens1 (RLis rendLis))
        where
            rendRs = rendCoda <$> rs
            rendLis = insertComma sep rendRs
            sep = spaceSymbol ","
    Cl (ClCat val) -> rendCoda (defConvert val TypeString)
    Str s -> entity (RStr s)
    Dir b ps -> entity (RLis [rendB, dirSep, rendP])
        where
            rendB = TightParens (rendCoda b)
            rendP = Symbol ps
            dirSep = Symbol "/"
    Let{} -> entity (uncurry RLet (getLetLis cv))
        where
            getLetLis :: CodaVal -> ([RendCoda], RendCoda)
            getLetLis (Let v1 val1 body1) =
                let 
                    enwAs = RLis [spaceSymbol v1, spaceSymbol "=", rendCoda val1]
                in
                    over _1 (enwAs:) (getLetLis body1)
            getLetLis other = ([], rendCoda other)
    Convert _ val ct -> entity (doubleColon rendVal (RType ct))
        where
            rendVal = case val of
                Let{} -> Parens1 (rendCoda val)
                _ -> rendCoda val
    Dict d -> entity (RDic (rendCoda <$> d))

randomPrintCoda :: CodaVal -> Gen String
randomPrintCoda = doRend . rendCoda