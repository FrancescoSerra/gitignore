module GitignoreSpec where

import CliApp
import Gitignore
import Test.Hspec

testReadIntoNumber :: Computation Int
testReadIntoNumber = readIntoNumber

testReadIntoNumberFail :: FailedComputation Int
testReadIntoNumberFail = readIntoNumber

-- addPatternFailed :: PlainPattern -> FailedComputation ()
-- addPatternFailed = addPattern


spec :: Spec
spec = parallel $ do
  helpers
  app

helpers :: Spec
helpers = do
  describe "readIntoNumber" $ do
    it "should read a number from stdin" $
      testReadIntoNumber `shouldBe` Result 1

    it "should handle a non-number input" $
      testReadIntoNumberFail `shouldBe` 
        FailedComputation (Error "Invalid input. Please enter a number.")

app :: Spec
app = describe "App" $ do
  context "addPattern" $ do
    it "should add a pattern to .gitignore" $ do
      let pattern = PlainPattern "pattern"
      addPattern pattern `shouldBe` Result ()

    it "should handle an existing pattern" $ do
      let pattern = PlainPattern "pattern"
      addPattern pattern `shouldBe` FailedComputation (Error ".gitignore")
  
  context "printOut" $ do
    it "should print the content of .gitignore" $
      printOut `shouldBe` Result ()
    
    it "should handle an error" $
      printOut `shouldBe` FailedComputation (Error ".gitignore")

  context "removePattern" $ do
    it "should remove a pattern from .gitignore" $
      removePattern `shouldBe` Result ()

    it "should handle an error" $
      removePattern `shouldBe` FailedComputation (Error ".gitignore")