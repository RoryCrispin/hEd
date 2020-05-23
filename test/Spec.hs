import Test.Hspec
import ParsEd
import Lib
import State
import Data.Map


dummyState :: State
dummyState = State [] 0 (fromList [('a', Line 1), ('b', Line 2)]) NormalMode

main :: IO ()
main = hspec $ do
  describe "Parse Target" $ do
    it "Parses numeric range" $ do
      parseTarget [TokNum 1, TokKey Comma, TokNum 2] emptyState `shouldBe` Left ((Line 1, Line 2), [])
      parseTarget [TokNum 10, TokKey Comma, TokNum 2] emptyState `shouldBe` Left ((Line 10, Line 2), [])

    it "Parses alpha and numeric tokens" $ do
      parseTarget [TokNum 1, TokKey  Comma, TokIdent 'a'] emptyState `shouldBe` Right "Invalid right hand target"
      parseTarget [TokIdent 'a', TokKey Comma, TokNum 1] emptyState `shouldBe` Right "Invalid left hand target"

    it "Parses single numgers" $ do
      parseTarget [TokNum 1, TokOp Print] emptyState `shouldBe` Left ((Line 1, Line 1), [TokOp Print])
      parseTarget [TokIdent 'a', TokOp Print] emptyState `shouldBe` Right "Invalid target"

  describe "Parse with reg table" $
    it "" $ do
      parseTarget [TokNum 2, TokKey  Comma, TokIdent 'a'] dummyState `shouldBe` Left ((Line 2, Line 1), [])
      parseTarget [TokIdent 'a', TokKey  Comma, TokNum 2] dummyState `shouldBe` Left ((Line 1, Line 2), [])
      parseTarget [TokIdent 'a', TokKey  Comma, TokIdent 'b'] dummyState `shouldBe` Left ((Line 1, Line 2), [])
