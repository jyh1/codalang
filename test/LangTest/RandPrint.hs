{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | random print an CodaVal to be parsed back
module LangTest.RandPrint where

import Lang.Types

import RIO
import RIO.List
import qualified RIO.Text as T
import Test.QuickCheck hiding (Result)

data RendCoda = Parens RendCoda 
    | Spaces RendCoda
    | RLis [RendCoda]
    | RStr String
    | Symbol String
    | RLit UUID
    deriving (Show, Read, Eq, Ord)

-- | symbol that could be surrounded with spaces
spaceSymbol :: String -> RendCoda
spaceSymbol = Spaces . Symbol

doRend :: RendCoda -> Gen String
doRend (RLit u) = return $ show u
doRend (RStr s) = return $ show s
doRend (RLis as) = concat <$> mapM doRend as
doRend (Symbol s) = return s
doRend (Spaces r) = doRend r >>= encloseSpaces
doRend (Parens r) = parens r >>= doRend


splitLists :: [a] -> Gen [[a]]
splitLists [] = return []
splitLists as = do
    k <- choose (1, len)
    let (fs, rs) = splitAt k as
    liftM (fs : ) (splitLists rs)
    where
        len = length as

sepEndBy :: a -> [a] -> Gen [a]
sepEndBy sep ss = do
    coin <- choose (True, False)
    if coin then return newStr else return (newStr ++ [sep])
    where
        newStr = intersperse sep ss

insertComma :: a -> [a] -> [a]
insertComma sep as = case as of
    [] -> error "Empty list"
    [a] -> [a, sep]
    _ -> intersperse sep as

-- | random white characters
spaces :: Gen String
spaces = listOf space
    where
        space = elements "\t \n"

encloseSpaces :: String -> Gen String
encloseSpaces s = liftA2 (\s1 s2 -> s1 ++ s ++ s2) spaces spaces

parens :: RendCoda -> Gen RendCoda
parens g = do
    dep <- choose (0, 5)
    let opens = replicate dep (spaceSymbol "(")
        closes = replicate dep (spaceSymbol ")")
    return (RLis (opens ++ [g] ++ closes))




-- tokenStr :: String -> Gen String
-- tokenStr s = token (return s)

-- rendCmd :: CodaCmd -> Gen String
-- rendCmd (Run cs) = case length cs of
--     1 -> 

-- rendVal :: CodaVal -> Gen String
-- rendVal (Lit u) = tokenStr (show u)
-- rendVal (Var v) = tokenStr (T.unpack v)
