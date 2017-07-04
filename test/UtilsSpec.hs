module UtilsSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified LogTree.Utils as LTU

spec :: Spec
spec = describe "LogTreeUtilsTest" $ do
  it "split empty" $ do
    LTU.splitLine ' ' "" `shouldBe` LTU.pack []
  it "split simple" $ do
    LTU.splitLine ' ' "some text" `shouldBe` LTU.pack ["some", "text"]
  it "split repeat" $ do
    LTU.splitLine ' ' " some  text  " `shouldBe` LTU.pack ["some", "text"]

  describe "isMonthTest" $ do
    it "numbers" $ do
      LTU.isMonth (T.pack "1") `shouldBe` True
      LTU.isMonth (T.pack "33") `shouldBe` True
      LTU.isMonth (T.pack "") `shouldBe` False
    it "letters" $ do
      LTU.isMonth (T.pack "apr") `shouldBe` True
      LTU.isMonth (T.pack "jea") `shouldBe` False

  describe "isDateTest" $ do
    it "move around" $ do
      LTU.isDate (T.pack "21-Apr-2015") `shouldBe` True
      LTU.isDate (T.pack "21-2015-Apr") `shouldBe` True
      LTU.isDate (T.pack "Apr-21-2015") `shouldBe` True
      LTU.isDate (T.pack "2015-Apr-21") `shouldBe` True
      LTU.isDate (T.pack "2015-21-Apr") `shouldBe` True
    it "malformed dates" $ do
      LTU.isDate (T.pack "apr-Apr-2015") `shouldBe` False
      LTU.isDate (T.pack "1001-10-2015") `shouldBe` False
