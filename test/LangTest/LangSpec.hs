{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LangTest.LangSpec(langSpec) where

import Test.Hspec
import Test.QuickCheck
import RIO
import RIO.Partial

import LangTest.Lang
import qualified RIO.Text as T

import RIO.Map (fromList)
import RIO.Partial (fromJust)
import Prelude (putStrLn)
import Lang.Types
import Data.Aeson
import Lang.JLang

langSpec :: Spec
langSpec = do
    parseSpec
    rcoSpec
    pprintSpec
    typeCheckSpec
    interpretInterface
    interpretJRes

parseSpec :: Spec
parseSpec = describe "CodaVal_parser" $ do
    let parseSucc s v = testParse s `shouldBe` Just v
    it "simple_expr" $ do
        parseSucc "0x5" 5
        parseSucc "aa/first/succ" (d "aa" ["first", "succ"])
        parseSucc "aa" "aa"
        parseSucc "\"string\"" (s "string")
    it "parened_expr" $ do
        parseSucc "  (0x05)" (l "05")
        parseSucc "(aa)" "aa"
        parseSucc "  ((aa))" "aa"
        parseSucc "(((((((aa)))))))" "aa"
        parseSucc "  (((aa)))/bb/c" (d "aa" ["bb", "c"])
        parseSucc "(((aa)))//bb/c" (d "aa" ["", "bb", "c"])
        parseSucc "(((aa))/)//bb/c" (d "aa" ["", "", "bb", "c"])
        parseSucc "(((aa/))/)//bb/c" (d "aa" ["", "", "", "bb", "c"])
    let aapy = d "aa" ["run.py"]
        aapyCmd = r [aapy]
    it "cmd_expr" $ do
        parseSucc "'$(0x0005/run.py)'" (r [d (l "0005") ["run.py"]])
        parseSucc "'$(0x00005/run.py) -k'" (clrun [ce (d (l "00005") ["run.py"]), p " -k"])
        parseSucc "'$(0x00005)/run.py -k'" (clrun [ce (l "00005"), p "/run.py -k"])
        parseSucc "'$(   0x00005)/run.py -k'" (clrun [ce (l "00005"), p "/run.py -k"])
        parseSucc "'$(   0x00005/run.py    )/run.py -k'" (clrun [ce (d (l "00005") ["run.py"]), p "/run.py -k"])
        parseSucc "' $(aa/run.py ) bb cc'/a" (d (clrun [p  " ", ce aapy, p " bb cc"]) ["a"])
        parseSucc "'$('$( 0x0005/run.py )') '" (clrun [ce (r [d (l "0005") ["run.py"]]), p " "])
        parseSucc "'$xy1/a'" (clrun [ce (v "xy1"), p "/a"])
    it "simple_let_expr" $ do
        parseSucc "let x = y in x" (clet "x" [("x", "y")])
        parseSucc "  let x=0x02 in x" (clet "x" [("x", (l "02"))])
        parseSucc "let x =  (aa/run.py); y=(aa/run.py) in x" (clet "x" [("x", aapy), ("y", aapy)])
    it "nested_let_expr" $ do
        parseSucc "let x = let y = 0x1 in y in x" (clet "x" [("x", clet "y" [("y", 1)])])
        parseSucc "'$(let x = 0x0002 in x/a/b)$(let x=0x01; y= \" x\" in '$x$y'/x)'"
            (r [clet (d "x" ["a", "b"]) [("x", (l "0002"))], clet (d (r ["x", "y"]) ["x"]) [("x", (l "01")), ("y", s " x")]])
    -- deprecated
    -- it "type_annotation" $ do
    --     let l2 = l "02"
    --         l2xy = d l2 ["x", "y"]
    --     parseSucc "0x02 as string" (cv l2 ts)
    --     parseSucc "0x02 as {}" (cv l2 emptBd)
    --     parseSucc "0x02 as bundle" (cv l2 abd)
    --     parseSucc "0x02/x/y as string" (cv l2xy ts)
    --     parseSucc "0x02/x/y as { a: string, b :{}}" (cv l2xy (bd [("a", ts), ("b", emptBd)]))
    --     parseSucc "0x02/x/y as { a: string, b :bundle}" (cv l2xy (bd [("a", ts), ("b", abd)]))
    it "dictionary" $ do
        parseSucc "{xx:0x1}" (dict [("xx", l "1")])
        parseSucc "{x:0x1, y:0x02/a}" (dict [("x", l "1"), ("y", d (l "02") ["a"])])
    -- parserQuickCheck

-- randomly generate codaval and randomly serilize to string, parser sould be able to parse it back
parserQuickCheck :: Spec
parserQuickCheck = describe "parser_quick_check" $ 
    it "parse back from randomly printed string" $ property $
        quickCheckWith stdArgs{ maxSuccess = 300 } (\(ParserTest cv cvStr) -> testParse cvStr == (Just cv))

pprintC = putStrLn . testPPrint
randomPrint = generate arbitrary >>= (\(RandCoda _ cv) -> pprintC cv)
-- pprint should be parsed back to the same AST
pprintSpec :: Spec
pprintSpec = describe "pretty-printer test" $ do
    let testPrinter tag printer = 
            it tag $ property $
                (\(RandCoda _ cv) -> testParse (printer cv) `shouldBe` Just cv) 
    testPrinter "full width" testPPrint
    testPrinter "default width" testPPrintShow
    testPrinter "compact" testPPrintCompact
                    

typeCheckSpec :: Spec
typeCheckSpec = describe "type-checker test" $ do
    it "pass type check in random generated ast" $ property $
        quickCheckWith stdArgs{ maxSuccess = 300 } (\(RandCoda ct cv) -> (testTypeCheck cv) `shouldBe` ct)
    it "same results of new AST" $ property $
        (\(RandCoda _ cv) -> checkInterpretRes (dummyInterpret cv) (dummyInterpret (testTypeCheckVal cv)))

rcoSpec :: Spec
rcoSpec = describe "RCO(remove_complex_operation)" $ do
    let simpRCOTest c v = testRCO c `shouldBe` v
        -- simpRCOTestSame c = simpRCOTest c c
    it "simple_let" $ do
        simpRCOTest (clet "x" [("x", 2)]) (clet "x-1" [("x-1", 2)])
        simpRCOTest (clet "x" [("x", 2), ("x", 3)]) (clet "x-2" [("x-2", 3)])
    it "nested_let" $ do
        simpRCOTest (clet "x" [("x", clet 1 [("y", 2)])]) (clet "x-2" [("x-2", 1)])
        simpRCOTest (clet "x" [("x", clet "y" [("y", clet "z" [("z", 2)])])]) 
            (clet "x-y-z-1" [("x-y-z-1", 2)])
    it "dir_expression" $ do
        let dirval k = d k ["c", "d"]
            letdir k v = clet (dirval (fromString k)) [(T.pack k, v)]
        simpRCOTest (letdir "a" 1) (letdir "a-1" 1)
        simpRCOTest (letdir "a" (d 1 ["m"])) (clet (dirval "a-2") [("a-1", 1), ("a-2", d "a-1" ["m"])])
    -- it "run_command" $ do
    --     let simpRun2 k1 k2 = r [s "aa", d k1 ["c", "d"], s "e", k2]
    --         simpRun k = simpRun2 k k
    --     simpRCOTest (simpRun 1) (clet (tmpNV 3) [(tmpN 1, 1), (tmpN 2, 1), (tmpN 3, simpRun2 (tmpNV 1) (tmpNV 2))])
    --     simpRCOTest (clet (simpRun "bb") [("bb", 2)]) ((clet (tmpNV 2) [("bb-1", 2), (tmpN 2, (simpRun "bb-1"))]))
    --     simpRCOTest (clet (simpRun "k") [("k", d 2 ["a"])]) 
    --         (clet (tmpNV 3) [("k-1", 2 ), ("k-2", d "k-1" ["a"]), (tmpN 3, simpRun "k-2")])
    it "random_gen_RCO" $ property $
        quickCheckWith stdArgs{ maxSuccess = 300 }(\(RandCodaRCO _ cv) -> checkER cv)
    it "same_result_after_RCO" $ property $
        quickCheckWith stdArgs{ maxSuccess = 300 }(\(RandCodaRCO old cv) -> snd (dummyInterpret old) `checkRes` snd (dummyInterpret cv))
    -- codaval -> typecheck -> RCO -> er -> pprint -> parse -> RCO -> er -> interpret (should produce same results)
    it "same results after parse back from RCO" $ property $
        quickCheckWith stdArgs{ maxSuccess = 300 } (\(RandCodaRCO old cv) -> snd (dummyInterpret old) `checkRes` snd (dummyInterpret (pipelined cv)))


doParse = fromJust . testParse
pipelined ast = testRCO (testTypeCheckVal (doParse (testPPrint ast)))


interpretInterface :: Spec
interpretInterface = do
    it "same_result_with_two_interpreters" $ property
        (\(RandCodaRCO old cv) -> 
            dummyInterpret cv == dummyInterpretWIntfrc cv)

interpretJRes :: Spec
interpretJRes = do
    it "same_result_with_two_interpreters" $ property
        (\(RandCodaRCO old cv) -> 
            dummyInterpret cv `shouldBe` dummyInterpretWJRes (testTypeCheck old) cv)

jlangJSONSpec :: Spec
jlangJSONSpec = do
    it "parse_to_and_from_JSON_encode" $ property
        (\(j :: JLang) -> j `shouldBe` (fromJust (decode (encode j))))
    it "parse_to_and_fromJSON" $ property
        (\(j :: JLang) -> j `shouldBe` (fromJust (decode (encode (toJSON j)))))
