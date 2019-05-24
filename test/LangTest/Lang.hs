{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module LangTest.Lang where

import Lang.Types
import Lang.Fold
import Lang.RCO

import RIO
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
  arbitrary = (UUID . abs) <$> (resize (2^40) arbitrary)

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
      num = choose ('0', '9')
  in
    T.pack <$> liftA2 (:) alpha (listOf (oneof [alpha, num]))

randLeaf :: CodaType -> GenEnv CodaVal
randLeaf c = case c of
  TypeBundle -> bundleLeaf
  TypeString -> strLeaf
  where
    genLeaf :: CodaType -> [Gen CodaVal] -> GenEnv CodaVal
    genLeaf t others = do
      gm <- use envL
      let vs = [v | (v, t') <- M.toList gm, t' == t]
      lift $ case vs of
        [] -> oneof others
        _ -> oneof ((Var <$> elements vs) : others)
    strLeaf :: GenEnv CodaVal
    strLeaf = genLeaf TypeString [Str <$> randVarName]
    bundleLeaf :: GenEnv CodaVal
    bundleLeaf = genLeaf TypeBundle [Lit <$> arbitrary]

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
  bdl <- decDepth (randTree TypeBundle)
  path <- nonEmptyList 5 (lift randVarName)
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

randTree :: CodaType -> GenEnv CodaVal
randTree t = do
  n <- use depth
  if n == 0 
    then randLeaf t 
    else case t of
      TypeBundle -> randBundle
      TypeString -> randString
  where
    -- non leaf bundle type
    randBundle :: GenEnv CodaVal
    randBundle = oneofGenEnv [randCl, randDir, randLet TypeBundle]
    -- non leaf string
    randString :: GenEnv CodaVal
    randString = randLet TypeString


randCodaVal :: GenEnv (CodaType, CodaVal)
randCodaVal = do
  t <- randType
  val <- randTree t
  return (t, val)
  where
    randType :: GenEnv CodaType
    randType = lift (elements [TypeString, TypeBundle])

data RandCoda = RandCoda CodaType CodaVal
    deriving (Show, Read, Eq)

instance Arbitrary RandCoda where
  arbitrary = uncurry RandCoda <$> sized sizeGen
    where
      sizeGen dep = evalStateT randCodaVal (VarEnv mempty (min 15 dep))


-- short functions for writing expression

instance IsString CodaVal where
  fromString = Var . T.pack

instance Num UUID where
  fromInteger = UUID

instance Num CodaVal where
  fromInteger = Lit . fromInteger

s :: Text -> CodaVal
s = Str
v :: VarName -> CodaVal
v = Var
c :: CodaCmd -> CodaVal
c = Cl
r = Cl . Run
d :: CodaVal -> [Text] -> CodaVal
d = foldl Dir
clet :: CodaVal -> [(Text, CodaVal)] -> CodaVal
clet = foldr (uncurry Let)
tmpN :: Int -> Text
tmpN n = tmpName <> "-" <> tshow n
tmpNV :: Int -> CodaVal
tmpNV = Var . tmpN

testRCO :: CodaVal -> CodaVal
testRCO = runRCO

type RCOCheck = CodaVal -> Either String ()
showError :: Show a => String -> a -> Either String b
showError t val = Left (t <> ": " <> show val)
-- RCO specification
isBundle :: RCOCheck
isBundle (Var _) = return ()
-- isBundle (Dir v _) = isDir v
isBundle v = showError "isBundle" v

isStr :: RCOCheck
isStr (Str _) = return ()
isStr v = showError "isStr" v

isValue :: RCOCheck
isValue x = msum [isBundle x, isStr x, showError "isValue" x]

isCMD :: RCOCheck
isCMD (Cl (Run as)) = msum (isValue <$> as)
isCMD v = showError "isCMD" v

isDir :: RCOCheck
isDir (Dir v _) = isBundle v `mplus` isLit v
isDir v = showError "isDir" v

isLit :: RCOCheck
isLit (Lit _) = return ()
isLit v = showError "isLit" v

isLet :: RCOCheck
isLet (Let _ val body) = msum ((($ val) <$> [isCMD, isDir, isLit, isStr]) ++ [isRCO body])
isLet v = showError "isLet" v

isRCO :: RCOCheck
isRCO v = isValue v `mplus` isLet v

checkRCO :: CodaVal -> Bool
checkRCO v = case isRCO v of 
  Right _ -> True
  Left s -> error s