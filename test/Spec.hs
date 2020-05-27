import Test.Hspec
import ParsEd
import Lib
import State
import Data.Map
import Eval


dummyState :: State
dummyState = State ["l1", "l2", "l3", "l4"] 0 (fromList [('a', Line 1), ('b', Line 2)]) NormalMode

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

    it "Parses the dollar as last line" $
      parseTarget [TokIdent '$'] dummyState `shouldBe` Left ((Line 3, Line 3), [])
    it "Parses the comma as first through last lines" $
      parseTarget [TokIdent ','] dummyState `shouldBe` Left ((Line 0, Line 3), [])

  describe "Parse with reg table" $
    it "" $ do
      parseTarget [TokNum 2, TokKey  Comma, TokIdent 'a'] dummyState `shouldBe` Left ((Line 2, Line 1), [])
      parseTarget [TokIdent 'a', TokKey  Comma, TokNum 2] dummyState `shouldBe` Left ((Line 1, Line 2), [])
      parseTarget [TokIdent 'a', TokKey  Comma, TokIdent 'b'] dummyState `shouldBe` Left ((Line 1, Line 2), [])

  describe "Print command" $ do
    it "Prints the target" $ do -- this is the mark command...
      ee "3kx" emptyState `shouldBe` (
        (Left (Command (Line 3, Line 3) Mark [TokIdent 'x']))
        , emptyState)
      ee "bp" dummyState `shouldBe` (
        (Left (Command (Line 2, Line 2) Print []))
        , dummyState)

    xit "Should accept registers with names conflicting with functions" $ do
      ee "3ka" emptyState `shouldBe` (
        (Left (Command (Line 3, Line 3) Mark [TokIdent 'a']))
        , emptyState)

  describe "Test evaluation" $ do
    it "prints" $ do
      evaluate (Command (Line 3, Line 3) Print []) dummyState `shouldBe` (dummyState, "l4\n")
    xit "Really it should index from 1 like ed. I will write no more\
        \ tests for the correct offset. These tests need to be updated when\
        \ I fix the offset" $ do
      evaluate (Command (Line 3, Line 3) Print []) dummyState `shouldBe` (dummyState, "l3\n")


