{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module LangTest.Lang where

import Lang.Types
import Lang.Fold

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
      cmdeles <- (`replicate` (snd <$> randCodaVal)) <$> lift (choose (1, 10))
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

instance Arbitrary CodaVal where
  arbitrary = sized (\d -> evalStateT (snd <$> randCodaVal) (VarEnv mempty d))