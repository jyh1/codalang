{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module LangTest.Lang where

import LangTest.Interpret (testInterpret, testInterpretWIntrfc)
import LangTest.RandPrint (randomPrintCoda)

import Lang.Types
import Lang.Fold
import Lang.RCO
import Lang.Parser
import Lang.PPrint
import Lang.TypeCheck(typeCheck)
import Data.Text.Prettyprint.Doc.Render.Text(renderStrict)
import Data.Text.Prettyprint.Doc (layoutCompact)
import           Numeric                        ( showHex )


import RIO
import RIO.List (delete)
import qualified RIO.Text as T
import qualified RIO.Map as M
import RIO.List (foldl, repeat)
import Test.QuickCheck hiding (Result)
import Control.Monad.State
import Control.Lens hiding (elements, lens)


-- random generation

data VarEnv = VarEnv {_varenv :: Map VarName CodaType, _depth :: Int}

makeLenses ''VarEnv

type GenEnv = StateT VarEnv Gen

instance Arbitrary UUID where
  arbitrary = (fromInteger . abs) <$> resize (2^40) arbitrary

instance HasEnv VarEnv CodaType where
  envL = varenv

instance LocalVar VarEnv GenEnv CodaType where


nonEmptyList :: Int -> GenEnv a -> GenEnv [a]
nonEmptyList n g = do
  k <- lift (choose (1, n))
  replicateM k g

oneofGenEnv :: [GenEnv a] -> GenEnv a
oneofGenEnv gs = join (lift (elements gs))


randVar :: GenEnv Text
randVar = lift $ frequency [
      (2, return "x")
    , (2, return "y")
    , (6, randVarName)
  ]
randVarName :: Gen Text
randVarName = 
  let alpha = choose ('a', 'z')
      varSymbol = elements ("." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  in
    T.pack <$> liftA2 (:) alpha (resize 1 (listOf varSymbol))

randLeaf :: CodaType -> GenEnv CodaVal
randLeaf c = case c of
  BundleDic{} -> bundleLeaf
  TypeString -> strLeaf
  where
    genLeaf :: (CodaType -> Bool) -> [Gen CodaVal] -> GenEnv CodaVal
    genLeaf f others = do
      gm <- use envL
      let vs = [v | (v, t') <- M.toList gm, f t']
      lift $ case vs of
        [] -> oneof others
        _ -> oneof ((Var <$> elements vs) : others)
    strLeaf :: GenEnv CodaVal
    strLeaf = genLeaf ( == TypeString) [Str <$> randStr]
        where
          randStr = T.pack <$> (resize 5 (listOf arbitraryASCIIChar))
    bundleLeaf :: GenEnv CodaVal
    bundleLeaf = genLeaf (isBundle) [Lit <$> arbitrary]
    isBundle t = case t of
      BundleDic{} -> True
      _ -> False

-- non-leaf node
childDepth :: (Int -> Int) -> GenEnv a -> GenEnv a
childDepth f gen = do
  curdep <- use depth
  depth %= f
  res <- gen
  depth .= curdep
  return res

halfDepth, decDepth :: GenEnv a -> GenEnv a
halfDepth = childDepth (`div` 2)
decDepth = childDepth (\x -> x - 1)

randDir :: GenEnv CodaVal
randDir = do
  bdl <- decDepth (randTree typeBundle)
  path <- lift (resize 5 (listOf randVarName))
  return (foldl Dir bdl path)

randCl :: GenEnv CodaVal
randCl = Cl <$> randCmd
  where
    randCmd :: GenEnv CodaCmd
    randCmd = do
      cmdeles <- (`replicate` (snd <$> randCodaVal)) <$> lift (choose (1, 6))
      geneles <- zipWithM ($) reduceDepth cmdeles
      return (Run geneles)
      where
        reduceDepth = decDepth : repeat halfDepth

randLet :: CodaType -> GenEnv CodaVal
randLet t = do
  varName <- randVar
  (vt, val) <- halfDepth randCodaVal
  body <- decDepth (withVar varName vt (randTree t))
  return (Let varName val body)

randConvert :: CodaType -> GenEnv CodaVal
randConvert t = do
  (_, val) <- decDepth randCodaVal
  return (Convert val t)

randTree :: CodaType -> GenEnv CodaVal
randTree t = do
  n <- use depth
  if n == 0 
    then randLeaf t 
    else case t of
      BundleDic{} -> randBundle t
      TypeString -> randString
  where
    -- non leaf bundle type
    randBundle :: CodaType -> GenEnv CodaVal
    randBundle t = oneofGenEnv [randCl, randDir, randLet t, randConvert t]
    -- non leaf string
    randString :: GenEnv CodaVal
    randString = oneofGenEnv [randLet TypeString, randConvert TypeString]


randCodaVal :: GenEnv (CodaType, CodaVal)
randCodaVal = do
  t <- lift randType
  val <- randTree t
  return (t, val)
  where
    randType :: Gen CodaType
    randType = oneof [pure TypeString, BundleDic <$> randDic]
    randDicEle = liftA2 (curry id) randVarName randType
    randDic = M.fromList <$> (resize 3 (listOf randDicEle))

randoCodaValWithDep :: Int -> Gen (CodaType, CodaVal)
randoCodaValWithDep dep = evalStateT randCodaVal (VarEnv mempty (min 15 dep))

data RandCoda = RandCoda CodaType CodaVal
    deriving (Show, Read, Eq)

instance Arbitrary RandCoda where
  arbitrary = uncurry RandCoda <$> sized randoCodaValWithDep

-- parser test
-- | randomly print a codaval AST, the parser should be able to parse back
data ParserTest = ParserTest CodaVal String
    deriving (Show, Read, Eq)
instance Arbitrary ParserTest where
  arbitrary = do
    (_, randCv) <- sized randoCodaValWithDep
    randStr <- randomPrintCoda randCv
    return (ParserTest randCv randStr)

-- short functions for writing expression

instance IsString CodaVal where
  fromString = Var . T.pack

instance Num UUID where
  fromInteger n = UUID (T.pack (showHex n ""))

instance Num CodaVal where
  fromInteger = Lit . fromInteger

s :: Text -> CodaVal
s = Str
v :: VarName -> CodaVal
v = Var
c :: CodaCmd -> CodaVal
c = Cl
r = Cl . Run
l = Lit . UUID
d :: CodaVal -> [Text] -> CodaVal
d = foldl Dir
clet :: CodaVal -> [(Text, CodaVal)] -> CodaVal
clet = foldr (uncurry Let)
tmpN :: Int -> Text
tmpN n = tmpName <> "-" <> tshow n
tmpNV :: Int -> CodaVal
tmpNV = Var . tmpN
bd = BundleDic . M.fromList
cv = Convert
ts = TypeString
emptBd = BundleDic mempty

normType :: CodaType -> CodaType
normType BundleDic{} = BundleDic mempty
normType t = t

testRCO :: CodaVal -> CodaVal
testRCO = runRCO

type RCOCheck = CodaVal -> Either String ()
showError :: Show a => String -> a -> Either String b
showError t val = Left (t <> ": " <> show val)
-- RCO specification
isBundle :: RCOCheck
isBundle v = msum [isDir v, isVar v]

isVar :: RCOCheck
isVar (Var _) = return ()
isVar v = showError "isVar" v

isStr :: RCOCheck
isStr (Str _) = return ()
isStr v = showError "isStr" v

isValue :: RCOCheck
isValue x = msum [isBundle x, isStr x, showError "isValue" x]

isCMD :: RCOCheck
isCMD (Cl (Run as)) = sequence_ (isValue <$> as)
isCMD v = showError "isCMD" v

isDir :: RCOCheck
isDir (Dir v _) = isBundle v
isDir v = showError "isDir" v

isLit :: RCOCheck
isLit (Lit _) = return ()
isLit v = showError "isLit" v

isLet :: RCOCheck
isLet (Let _ val body) = sequence_ [msum (($ val) <$> [isCMD, isDir, isLit, isStr]), isRCO body]
isLet v = showError "isLet" v

isRCO :: RCOCheck
isRCO v = isValue v `mplus` isLet v

checkRCO :: CodaVal -> Bool
checkRCO v = case isRCO v of 
  Right _ -> True
  Left s -> error s

dummyInterpret = testInterpret
dummyInterpretWIntfrc = testInterpretWIntrfc

testParse :: String -> Maybe CodaVal
testParse = loadString


testPPrint :: CodaVal -> String
testPPrint = T.unpack . pprintCoda

testPPrintShow :: CodaVal -> String
testPPrintShow = show . codaToDoc

testPPrintCompact :: CodaVal -> String
testPPrintCompact cv = T.unpack (renderStrict (layoutCompact (codaToDoc cv)))

testTypeCheck :: CodaVal -> Either Text CodaType
testTypeCheck = typeCheck