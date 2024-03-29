{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module LangTest.Lang where

import LangTest.Interpret
import LangTest.RandPrint (randomPrintCoda)

import Lang.Types
import Lang.Fold
import Lang.RCO
import Lang.Parser
import Lang.PPrint
import Lang.TypeCheck(typeCheck)
import Lang.JLang


import Data.Text.Prettyprint.Doc.Render.Text(renderStrict)
import Data.Text.Prettyprint.Doc (layoutCompact)
import           Numeric                        ( showHex )
import RIO
import RIO.Char (isLetter, isDigit)
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


genVar :: Gen Text
genVar = frequency [
      (2, return "x")
    , (2, return "y")
    , (6, randVarName)
  ]
randVar :: GenEnv Text
randVar = lift genVar
randVarName :: Gen Text
randVarName = 
  let alpha = choose ('a', 'z')
      varSymbol = elements ("." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  in
    T.pack <$> liftA2 (:) alpha (resize 1 (listOf varSymbol))

randLeaf :: CodaType -> GenEnv CodaVal
randLeaf c = genLeaf c (constLeaf c)
  where
    genLeaf :: CodaType -> [GenEnv CodaVal] -> GenEnv CodaVal
    genLeaf t others = do
      gm <- use envL
      let vs = [v | (v, t') <- M.toList gm, t' == t]
      case vs of
        [] -> oneofGenEnv others
        _ -> oneofGenEnv ((lift (Var <$> elements vs)) : others)
    constLeaf :: CodaType -> [GenEnv CodaVal]
    constLeaf c = case c of
      TypeBundle -> [lift (Lit <$> arbitrary)]
      TypeRecord td -> [Dict <$> mapM randLeaf td]
      TypeString -> [lift (Str <$> randStr)]
        where
          randStr = T.pack <$> (resize 5 (listOf arbitraryASCIIChar))
      TypeLam td ret -> [
        do
          newN <- randVar
          fun <- Lambda td <$> sandBox (envL %= M.union td >> randLeaf ret)
          return fun
        ]
      

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
decDepth = childDepth (\x -> (x - 1) `max` 0)

randDir :: CodaType -> GenEnv CodaVal
randDir t = do
  (tdic, var) <- lift (randTypeDicWith t)
  bdl <- decDepth (randTree tdic)
  return (Dir bdl var)
  where
    randTypeDicWith :: CodaType -> Gen (CodaType, VarName)
    randTypeDicWith t = do
      newN <- randDicKey
      let dicVal = (TypeRecord . M.insert newN t) <$> randTypeDic
          dicGen = case t of
            TypeBundle -> [dicVal, pure TypeBundle]
            _          -> [dicVal]
      ty <- oneof dicGen
      return (ty, newN)

randCl :: GenEnv CodaVal
randCl = do
  opts <- randClEle 0 [TypeString]
  recBundle <- lift (randTypeDicOf (return TypeBundle))
  runcmd <- Run <$> randClEle 1 [TypeBundle, TypeString, TypeRecord recBundle]
  return (Cl opts runcmd)

randClEle :: Int -> [CodaType] -> GenEnv (OptEnv CodaVal)
randClEle n ts = randCmd
  where
    randCmd :: GenEnv (OptEnv CodaVal)
    randCmd = do
      let
        randCmdExpr = CMDExpr <$> (pickType >>= randTree)
        notVarChar c = not (isLetter c || isDigit c || c == '.' || c == '-' || c == '_')
        plainStr = liftA2 (:) (arbitraryPrintableChar `suchThat` notVarChar) (resize 5 (listOf arbitraryPrintableChar))
        randCmdPlain = lift ((Plain . T.pack) <$> plainStr)
        twoEle a b = liftA2 (\t1 t2 -> [t1, t2]) a b
        plainExpr = twoEle randCmdPlain randCmdExpr
        exprExpr = twoEle randCmdExpr randCmdExpr
        randCmdEles = oneofGenEnv [plainExpr, exprExpr]
      cmdeles <- (`replicate` randCmdEles) <$> lift (choose (n, 3))
      geneles <- zipWithM ($) reduceDepth cmdeles
      return (concat geneles)
      where
        pickType = lift (elements ts)
        reduceDepth = decDepth : decDepth : repeat halfDepth

randLet :: CodaType -> GenEnv CodaVal
randLet t = do
  varName <- lift randAssign
  (vt, val) <- halfDepth randCodaVal
  body <- withVar varName vt (randTree t)
  return (Let varName val body)
  where
    randAssign :: Gen AssignForm
    randAssign = genVar

-- disable generate convert node
randConvert :: CodaType -> GenEnv CodaVal
randConvert t =
  decDepth (randTree t)
-- randConvert :: CodaType -> GenEnv CodaVal
-- randConvert t = do
--   let genType = extendType t
--           where
--             extendType t = case t of
--               TypeString -> elements [TypeString, TypeBundle]
--               TypeBundle -> randNoTypeLam
--               TypeRecord d -> oneof (bool [] [pure TypeBundle] (notLambda t) ++ [TypeRecord <$> mapM extendType d])
--               TypeLam{} -> specialize t
--   newt <- lift genType
--   val <- decDepth (randTree newt)
--   return (defConvert val t)

specialize :: CodaType -> Gen CodaType
specialize t = case t of
  TypeRecord d -> TypeRecord <$> specializeDic d
  TypeLam dt ret -> liftA2 TypeLam (generalizeDic dt) (return ret)
  t -> return t
generalizeDic, specializeDic :: TypeDict -> Gen TypeDict
generalizeDic dt = newdic >>= (mapM generalize)
  where
    newdic = M.fromList <$> sublistOf (M.toList dt)
specializeDic dt = liftA2 M.union (mapM specialize dt) randTypeDic
generalize :: CodaType -> Gen CodaType
generalize t = case t of
  TypeRecord d -> TypeRecord <$> generalizeDic d
  TypeLam dt ret -> liftA2 TypeLam (specializeDic dt) (generalize ret)
  t -> return t
randValMap :: TypeDict -> GenEnv (TextMap CodaVal)
randValMap d = 
  (sequence $ M.fromList 
    [(k, dec (randTree t)) 
      | ((k, t), dec) <- zip (M.toList d) (decDepth : repeat halfDepth)])

randValDict :: TypeDict -> GenEnv CodaVal
randValDict dt = Dict <$> randValMap dt

-- randLam :: TypeDict -> CodaType -> GenEnv (CodaType, CodaVal)
randLam :: TypeDict -> CodaType -> GenEnv CodaVal
randLam ad ret = sandBox $ do
  -- ad <- lift randTypeDic
  -- ret <- lift randType
  envL %= M.union ad
  body <- decDepth $ randTree ret
  return (Lambda ad body)

randApp :: CodaType -> GenEnv CodaVal
randApp t = do
  argdic <- lift randTypeDic
  fun <- decDepth (randTree (TypeLam argdic t))
  argVal <- halfDepth (randValMap argdic)
  return (Apply fun argVal)


randTree :: CodaType -> GenEnv CodaVal
randTree t = do
  n <- use depth
  if n == 0 
    then randLeaf t 
    else case t of
      TypeRecord d -> randRecord d
      TypeString -> randString
      TypeBundle -> randBundle
      TypeLam dt ret -> randLam dt ret
  where
    -- non leaf bundle type
    randRecord :: TypeDict -> GenEnv CodaVal
    randRecord dic = 
      oneofGenEnv ([randDir t, randLet t, randConvert t, randApp t] ++ genValDic)
      where
        t = TypeRecord dic
        genValDic
          | M.null dic = []
          | otherwise = [randValDict dic]
    randBundle = oneofGenEnv [randCl, randDir t, randLet t, randConvert t, randApp t]
    -- non leaf string
    randString :: GenEnv CodaVal
    randString = oneofGenEnv [randLet TypeString, randConvert TypeString, randDir TypeString, randApp t]
    randLambda :: TypeDict -> CodaType -> GenEnv CodaVal
    randLambda dt ret = oneofGenEnv [randLet t, randConvert t, randDir t, randApp t]
        where
          t = TypeLam dt ret


randType :: Gen CodaType
randType = 
  frequency [
      (3, pure TypeString)
    , (3, pure TypeBundle)
    , (2, TypeRecord <$> randTypeDic)
    , (3, liftA2 TypeLam randTypeDic randType)
  ]
randDicKey = frequency ((1, randVarName) : [(2, pure ("key" <> tshow i)) | i <- [1..3]])
randDicEleOf rt = liftA2 (curry id) randDicKey rt
-- randDicEle = randDicEleOf randType
randTypeDicOf rt = M.fromList <$> (resize 3 (listOf (randDicEleOf rt)))
randTypeDic = randTypeDicOf randType


randNoTypeLam :: Gen CodaType
randNoTypeLam = suchThat randType notLambda
notLambda :: CodaType -> Bool
notLambda TypeLam{} = False
notLambda (TypeRecord dt) = allOf traverse notLambda dt
notLambda _ = True

randCodaVal :: GenEnv (CodaType, CodaVal)
randCodaVal = do
  t <- lift randType
  val <- randTree t
  return (t, val)

randoCodaValWithDep :: Int -> Gen (CodaType, CodaVal)
randoCodaValWithDep dep = evalStateT randCodaTest (VarEnv mempty (min 15 dep))
  where
    randCodaTest = do
      t <- lift randNoTypeLam
      val <- randTree t
      return (t, val)

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

instance Arbitrary JLang where
  arbitrary = do
    RandCodaRCO _ cv <- arbitrary
    return (toJLang cv)

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
c = makeCl
r = clrun . (map CMDExpr)
clrun = makeCl . Run
p = Plain
ce = CMDExpr
l = Lit . UUID
d :: CodaVal -> [Text] -> CodaVal
d = foldl Dir
clet :: CodaVal -> [(Text, CodaVal)] -> CodaVal
clet = foldr (\(k, v) -> Let k v)
tmpN :: Int -> Text
tmpN n = tmpName <> "-" <> tshow n
tmpNV :: Int -> CodaVal
tmpNV = Var . tmpN
bd = TypeRecord . M.fromList
abd = TypeBundle
cv = Convert Nothing
conv = Convert
ts = TypeString
emptBd = TypeRecord mempty
dict = Dict . M.fromList

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

isRunCmdEle :: RCOCheck
isRunCmdEle x = msum [isVar x, isStr x, isRecord x, showError "isValue" x]

isDir :: RCOCheck
isDir (Dir v _) = isBundle v
isDir v = showError "isDir" v

isLit :: RCOCheck
isLit (Lit _) = return ()
isLit v = showError "isLit" v

isConvert :: RCOCheck
isConvert c@(Convert f v t) = case (f, t) of
  (Just TypeString, TypeBundle) -> msum [isStr v, isVar v, isDir v, err]
  (Just TypeRecord{}, TypeBundle) -> err
  (_, TypeRecord{}) -> err
  _ -> isVar v
  where
    err = showError "isConvert" c
-- isConvert c = showError "isConvert" c
isConvert c
  -- | trace (T.pack $ testPPrint c) False = return ()
  | True = showError "isConvert" c


isRecord :: RCOCheck
isRecord (Dict d) = mapM_ isValue d
isRecord v = showError "isRecord" v


-- elminate record check
isERRet :: RCOCheck
isERRet (Dict d) = mapM_ isERRet d
isERRet v = isValue v

isERRes :: RCOCheck
isERRes (Let _ val body) = sequence_ [msum (($ val) <$> [isCMD, isDir, isLit, isStr, isConvert]), isERRes body]
  where
    isCMD :: RCOCheck
    isCMD (Cl _ cmd) = case cmd of
      Run runeles -> do 
        _ <- (traverse . cmdExpr) isRunCmdEle runeles
        return ()
      _ -> traverse_ isBundle cmd
    isCMD v = showError "isCMD" v
    isConvert c@(Convert _ v ty) = case ty of
      TypeString -> isValue v
      TypeBundle -> isValue v
      _ -> showError "isConvert" c
    isConvert c = showError "isConvert" c
isERRes v = isERRet v

checkER :: CodaVal -> Bool
checkER v = case isERRes v of 
  Right _ -> True
  Left s -> error s

dummyInterpret = testInterpret
dummyInterpretWIntfrc = testInterpretWIntrfc
dummyInterpretWJRes = testInterpretWJRes

checkInterpretRes :: (Eq a) => (a, CodaTestRes) ->  (a, CodaTestRes) -> Bool
checkInterpretRes (l1s, r1) (l2s, r2) = 
  l1s == l2s && checkRes r1 r2

checkRes (DictRes d1) (DictRes d2) = 
  dictMinus (flip checkRes) d1 d2
checkRes (DictRes d1) newval =
  and [checkRes v (makeDir newval k) |(k, v) <- M.toList d1]
checkRes (ResLam{}) (ResLam{}) = True
checkRes res1 res2 = res1 == res2

instance LoadModule Maybe where
  loadModule _ = error "load module in test"
  parseError _ _ e = error e

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


