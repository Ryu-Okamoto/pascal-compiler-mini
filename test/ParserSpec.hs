module ParserSpec ( spec ) where

import Test.Hspec ( Spec, SpecWith, describe, it, runIO, shouldBe )

import Token ( tkFileToTokens )
import Parser.ParseMonad ( Parse (..) )
import qualified Parser.Parser as Parser ( run )

spec :: Spec
spec = 
  describe "Parser.hs" $ do
    testCase "normal01" "OK"
    testCase "normal02" "OK"
    testCase "normal03" "OK"
    testCase "normal04" "OK"
    testCase "normal05" "OK"
    testCase "normal06" "OK"
    testCase "normal07" "OK"
    testCase "normal08" "OK"
    testCase "normal09" "OK"
    testCase "normal10" "OK"
    testCase "normal11" "OK"
    testCase "normal12" "OK"
    testCase "normal13" "OK"
    testCase "normal14" "OK"
    testCase "normal15" "OK"
    testCase "normal16" "OK"
    testCase "normal17" "OK"
    testCase "normal18" "OK"
    testCase "normal19" "OK"
    testCase "normal20" "OK"
    testCase "semerr01" "OK"
    testCase "semerr02" "OK"
    testCase "semerr03" "OK"
    testCase "semerr04" "OK"
    testCase "semerr05" "OK"
    testCase "semerr06" "OK"
    testCase "semerr07" "OK"
    testCase "semerr08" "OK"
    testCase "synerr01" "SyntaxError 1"
    testCase "synerr02" "SyntaxError 3"
    testCase "synerr03" "SyntaxError 8"
    testCase "synerr04" "SyntaxError 10"
    testCase "synerr05" "SyntaxError 11"
    testCase "synerr06" "SyntaxError 13"
    testCase "synerr07" "SyntaxError 30"
    testCase "synerr08" "SyntaxError 31"

testCase :: String -> String -> SpecWith ()
testCase baseName expected = do
  tokens <- runIO $ tkFileToTokens tkFilePath 
  let actual =
        case parseResult of
          (SyntaxError line) -> "SyntaxError " ++ show line
          (Parse       _   ) -> "OK"
        where
          parseResult = Parser.run tokens
  it ("identifies whether a syntax error exists in " ++ tkFile) $ actual `shouldBe` expected
  where
    tkFilePath = "./test/data/tk/" ++ tkFile 
    tkFile = baseName ++ ".tk"