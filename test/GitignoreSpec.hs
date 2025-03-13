module GitignoreSpec where

import CliApp
import Data.Functor.Identity
import Gitignore
import Test.Hspec

testReadIntoNumber :: Identity Int
testReadIntoNumber = readIntoNumber

testReadIntoNumberFail :: Computation Int
testReadIntoNumberFail = readIntoNumber

spec :: Spec
spec = do
  describe "readIntoNumber" $ do
    it "should read a number from stdin" $
      testReadIntoNumber `shouldBe` 1

    it "should handle a non-number input" $
      testReadIntoNumberFail `shouldBe` Error "Invalid input. Please enter a number."