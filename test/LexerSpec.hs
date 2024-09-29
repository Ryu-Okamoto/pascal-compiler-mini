module LexerSpec ( spec ) where

import Test.Hspec ( Spec, SpecWith, describe, it, runIO, shouldBe )

import Token ( tkFile2Tokens )
import qualified Lexer.Lexer as Lexer ( run )

spec :: Spec
spec = 
  describe "Lexer.hs" $ do
    testCase "normal01"
    testCase "normal02"
    testCase "normal03"
    testCase "normal04"
    testCase "normal05"
    testCase "normal06"
    testCase "normal07"
    testCase "normal08"
    testCase "normal09"
    testCase "normal10"
    testCase "normal11"
    testCase "normal12"
    testCase "normal13"
    testCase "normal14"
    testCase "normal15"
    testCase "normal16"
    testCase "normal17"
    testCase "normal18"
    testCase "normal19"
    testCase "normal20"
    testCase "semerr01"
    testCase "semerr02"
    testCase "semerr03"
    testCase "semerr04"
    testCase "semerr05"
    testCase "semerr06"
    testCase "semerr07"
    testCase "semerr08"
    testCase "synerr01"
    testCase "synerr02"
    testCase "synerr03"
    testCase "synerr04"
    testCase "synerr05"
    testCase "synerr06"
    testCase "synerr07"
    testCase "synerr08"
    

testCase :: String -> SpecWith ()
testCase baseName = do
  actual <- runIO $ Lexer.run pasFilePath
  expected <- runIO $ tkFile2Tokens tkFilePath
  it ("converts " ++ pasFile ++ " to " ++ tkFile) $ actual `shouldBe` expected
  where
    pasFilePath = "./test/data/pas/" ++ pasFile
    tkFilePath = "./test/data/tk/" ++ tkFile 
    pasFile = baseName ++ ".pas"
    tkFile = baseName ++ ".tk"