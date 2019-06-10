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
  BundleDic td -> bundleLeaf td
  TypeString -> strLeaf
  where
    genLeaf :: (CodaType -> Bool) -> [GenEnv CodaVal] -> GenEnv CodaVal
    genLeaf f others = do
      gm <- use envL
      let vs = [v | (v, t') <- M.toList gm, f t']
      case vs of
        [] -> oneofGenEnv others
        _ -> oneofGenEnv ((lift (Var <$> elements vs)) : others)
    strLeaf :: GenEnv CodaVal
    strLeaf = genLeaf ( == TypeString) [lift (Str <$> randStr)]
        where
          randStr = T.pack <$> (resize 5 (listOf arbitraryASCIIChar))
    bundleLeaf :: TypeDict -> GenEnv CodaVal
    bundleLeaf td = genLeaf isBundle leafDict
      where
        isBundle (BundleDic k) = td == k
        isBundle _ = False
        leafDict = case td of
          TAll -> [lift (Lit <$> arbitrary)]
          TDict k -> [Dict <$> mapM randLeaf k]

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

randDir :: CodaType -> GenEnv CodaVal
randDir t = do
  (tdic, var) <- lift (randTypeDicWith t)
  bdl <- decDepth (randTree (BundleDic tdic))
  return (Dir bdl var)

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
  newt <- lift randType
  val <- decDepth (randTree newt)
  return (Convert val t)
  -- where
    -- convertable :: CodaType -> Gen CodaType
    -- convertable TypeString = randType
    -- convertable (BundleDic d) = frequency [(1, return TypeString), (5, BundleDic <$> extendDict d)]
    
extendDict :: TypeDict -> Gen TypeDict
extendDict d = case d of
  TAll -> return TAll
  TDict dict -> frequency [(3, TDict . (dict `M.union`) <$> randTypeDic), (3, pure d), (1, pure TAll)]

randValDict :: Map Text CodaType -> GenEnv CodaVal
randValDict d = 
  Dict <$> (sequence $ M.fromList 
    [(k, dec (randTree t)) 
      | ((k, t), dec) <- zip (M.toList d) (decDepth : repeat halfDepth)])

randTree :: CodaType -> GenEnv CodaVal
randTree t = do
  n <- use depth
  if n == 0 
    then randLeaf t 
    else case t of
      BundleDic d -> randBundle d
      TypeString -> randString
  where
    -- non leaf bundle type
    randBundle :: TypeDict -> GenEnv CodaVal
    randBundle d = case d of
      TAll -> oneofGenEnv [randCl, randDir t, randLet t, randConvert t]
      TDict dic -> oneofGenEnv [randValDict dic, randDir t, randLet t, randConvert t]
      where
        t = BundleDic d
    -- non leaf string
    randString :: GenEnv CodaVal
    randString = oneofGenEnv [randLet TypeString, randConvert TypeString, randDir TypeString]


randType :: Gen CodaType
randType = oneof [pure TypeString, pure (BundleDic TAll), BundleDic . TDict <$> randTypeDic]
randDicKey = frequency ((1, randVarName) : [(2, pure ("key" <> tshow i)) | i <- [1..3]])
randDicEle = liftA2 (curry id) randDicKey randType
randTypeDic = M.fromList <$> (resize 3 (listOf randDicEle))
randTypeDicWith :: CodaType -> Gen (TypeDict, VarName)
randTypeDicWith t = do
  newN <- randDicKey
  let dicVal = (TDict . M.insert newN t) <$> randTypeDic
  dic <- dicVal
  return (dic, newN)

randCodaVal :: GenEnv (CodaType, CodaVal)
randCodaVal = do
  t <- lift randType
  val <- randTree t
  return (t, val)

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

-- arbitrary RandCoda after type check
data RandCodaTypeCheck = RandCodaTypeCheck CodaVal CodaVal
    deriving (Show, Read, Eq)
instance Arbitrary RandCodaTypeCheck where
  arbitrary = do
    (RandCoda _ cv) <- arbitrary
    return (RandCodaTypeCheck cv (testTypeCheckVal cv))

-- rcoed cv
data RandCodaRCO = RandCodaRCO CodaVal CodaVal
      deriving (Show, Read, Eq)
instance Arbitrary RandCodaRCO where
  arbitrary = do
    RandCodaTypeCheck old cv <- arbitrary
    return (RandCodaRCO old (testRCO cv))

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
bd = BundleDic . TDict . M.fromList
abd = BundleDic TAll
cv = Convert
ts = TypeString
emptBd = BundleDic (TDict mempty)
dict = Dict . M.fromList


normType :: CodaType -> CodaType
normType BundleDic{} = BundleDic TAll
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
isCMD (Cl (ClCat v)) = msum [isValue v]
isCMD v = showError "isCMD" v

isDir :: RCOCheck
isDir (Dir v _) = isBundle v
isDir v = showError "isDir" v

isLit :: RCOCheck
isLit (Lit _) = return ()
isLit v = showError "isLit" v

isConvert :: RCOCheck
isConvert c@(Convert v t) = case (v, t) of
  (Var{}, BundleDic{}) -> return ()
  _ -> showError "isConvert" c
isConvert c = showError "isConvert" c

isLet :: RCOCheck
isLet (Let _ val body) = sequence_ [msum (($ val) <$> [isCMD, isDir, isLit, isStr, isConvert]), isRCO body]
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

testTypeCheck :: CodaVal -> CodaType
testTypeCheck c = either (error . T.unpack) fst (typeCheck c)

testTypeCheckVal :: CodaVal -> CodaVal
testTypeCheckVal c = either (error . T.unpack) snd (typeCheck c)

typeCompat :: CodaType -> CodaType -> Bool
typeCompat = isSubtypeOf