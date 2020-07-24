{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Text as T

import Test.Hspec
import ParsEd
import Lib
import State
import Data.Map
import Eval


dummyState :: State
dummyState = State ["l1", "l2", "l3", "l4"] 0 (fromList [('a', Line 1), ('b', Line 2), ('p', Line 2)]) NormalMode ""
dummyState2 :: State
dummyState2 = State ["One", "Two", "Three", "Four", "Five", "Six"] 2 (fromList [('a', Line 1), ('b', Line 2), ('p', Line 2)]) NormalMode ""

main :: IO ()
main = hspec $ do
  describe "Test tokenize" $ do
    it "parses registers" $
      tokenize "'ap3'c" `shouldBe` [TokReg 'a', TokChar 'p', TokNum 3, TokReg 'c']
    it "Tokenizes offset targets" $ do
      tokenize "+p" `shouldBe` [TokOffset 1, TokChar 'p']
      tokenize "-p" `shouldBe` [TokOffset (-1), TokChar 'p']
      tokenize "+1p" `shouldBe` [TokOffset 1, TokChar 'p']
      tokenize "-1p" `shouldBe` [TokOffset (-1), TokChar 'p']
      tokenize "+12345p" `shouldBe` [TokOffset 12345, TokChar 'p']
      tokenize "-12345p" `shouldBe` [TokOffset (-12345), TokChar 'p']
    it "Tokenizes move command" $
      tokenize "1,2t4" `shouldBe` [TokNum 1, TokKey Comma, TokNum 2, TokChar 't', TokNum 4]
  describe "Parse Target" $ do
    it "Parses numeric range" $ do
      parseTarget [TokNum 1, TokKey Comma, TokNum 2] emptyState `shouldBe` Right ((Line 1, Line 2), [], emptyState)
      parseTarget [TokNum 10, TokKey Comma, TokNum 2] emptyState `shouldBe` Right ((Line 10, Line 2), [], emptyState)

    it "Parses alpha and numeric tokens" $ do
      parseTarget [TokNum 1, TokKey  Comma, TokChar 'a'] emptyState `shouldBe` Left "Invalid right hand target"
      parseTarget [TokChar 'a', TokKey Comma, TokNum 1] emptyState `shouldBe` Left "Invalid left hand target"

    it "Parses single numgers" $ do
      parseTarget [TokNum 1, TokChar 'p'] emptyState `shouldBe` Right ((Line 1, Line 1), [TokChar 'p'], emptyState)
      parseTarget [TokChar 'a', TokChar 'p'] emptyState `shouldBe` Left "Invalid target"

    it "Parses the dollar as last line" $
      parseTarget [TokChar '$'] dummyState `shouldBe` Right ((Line 3, Line 3), [], dummyState)
    xit "Parses the comma as first through last lines" $
      parseTarget [TokChar ','] dummyState `shouldBe` Right ((Line 0, Line 3), [], dummyState)

  describe "Parse with reg table" $
    it "" $ do
      parseTarget [TokNum 2, TokKey  Comma, TokChar 'a'] dummyState `shouldBe` Right ((Line 2, Line 1), [], dummyState)
      parseTarget [TokChar 'a', TokKey  Comma, TokNum 2] dummyState `shouldBe` Right ((Line 1, Line 2), [], dummyState)
      parseTarget [TokChar 'a', TokKey  Comma, TokChar 'b'] dummyState `shouldBe` Right ((Line 1, Line 2), [], dummyState)

  describe "Print command" $ do
    it "Prints the target" $ do -- this is the mark command...
      ee "3kx" emptyState `shouldBe` Right (Command (Line 3, Line 3) Mark [TokChar 'x'], emptyState)
      ee "bp" dummyState `shouldBe` Right (Command (Line 2, Line 2) Print [], dummyState)
      ee "+3p" dummyState `shouldBe` Right (Command (Line 3, Line 3) Print [], dummyState)
      ee "-3p" dummyState `shouldBe` Right (Command (Line (-3), Line (-3)) Print [], dummyState)
      ee "$p" dummyState `shouldBe` Right (Command (Line 3, Line 3) Print [], dummyState)
      ee ",p" dummyState `shouldBe` Right (Command (Line 0, Line 3) Print [], dummyState)
      ee ";p" dummyState `shouldBe` Right (Command (Line 0, Line 3) Print [], dummyState)
      ee "1,2mj" dummyState `shouldBe` Right (Command (Line 1, Line 2) Move [TokChar 'j'], dummyState)
      ee "1,2tj" dummyState `shouldBe` Right (Command (Line 1, Line 2) Transfer [TokChar 'j'], dummyState)
      ee "1,2m3" dummyState `shouldBe` Right (Command (Line 1, Line 2) Move [TokNum 3], dummyState)
      ee "1,2t3" dummyState `shouldBe` Right (Command (Line 1, Line 2) Transfer [TokNum 3], dummyState)

    it "Should accept registers with names conflicting with functions" $
      ee "3ka" emptyState `shouldBe` (
        Right ((Command (Line 3, Line 3) Mark [TokChar 'a'])
        , emptyState))
    it "Should accept register notation in otherwise ambiguous commands" $
      ee "'pp" dummyState `shouldBe` (
        Right ((Command (Line 2, Line 2) Print [])
        , dummyState))
  describe "Number command" $
    it "evaluates" $
      evaluate (Command (Line 3, Line 3) Number []) dummyState `shouldBe` (dummyState, "3 l4\n")

  describe "After command" $
    it "evaluates" $
      evaluate (Command {target = (Line 3,Line 3), op = After, params = []}) dummyState `shouldBe` (
        dummyState {position = 4, mode = InsertMode},">")
  describe "Insert command" $
    it "evaluates" $
      evaluate (Command {target = (Line 3,Line 3), op = Insert, params = []}) dummyState `shouldBe` (
        dummyState {position = 3, mode = InsertMode},">")
  describe "Change command" $
    it "evaluates" $
      -- TODO how does deleting/changing a range move the current target?
      evaluate (Command {target = (Line 3,Line 3), op = Change, params = []}) dummyState `shouldBe` (
        dummyState {buffer = ["l1","l2","l3"], mode = InsertMode},">")
  describe "Delete command" $
    it "evaluates" $
      evaluate (Command {target = (Line 3,Line 3), op = Delete, params = []}) dummyState `shouldBe` (
        dummyState {buffer = ["l1","l2","l3"], mode = NormalMode},"OK")
  describe "Join command" $
    it "evaluates" $
      evaluate (Command {target = (Line 0,Line 1), op = Join, params = []}) dummyState `shouldBe` (
        dummyState {buffer = ["l1l2","l3","l4"]},"OK")
  describe "Move command" $
    it "evaluates" $
      evaluate (Command {target = (Line 0,Line 1), op = Move, params = [TokNum 2]}) dummyState `shouldBe` (
        dummyState {buffer = ["l3","l1","l2","l4"]},"OK")
  describe "Transfer command" $
    it "evaluates" $
      evaluate (Command {target = (Line 0,Line 1), op = Transfer, params = [TokNum 2]}) dummyState `shouldBe` (
        dummyState {buffer = ["l1","l2","l1","l2","l3","l4"]}, "OK")

  describe "Test evaluation" $ do
    it "prints" $
      evaluate (Command (Line 3, Line 3) Print []) dummyState `shouldBe` (dummyState, "l4\n")
    xit "Really it should index from 1 like ed. I will write no more\
        \ tests for the correct offset. These tests need to be updated when\
        \ I fix the offset" $
      evaluate (Command (Line 3, Line 3) Print []) dummyState `shouldBe` (dummyState, "l3\n")

  describe "Test Regexp" $ do
    it "parses regexp strings" $ do
      tokenize "/some string/p" `shouldBe` [TokRegexp "some string", TokChar 'p']
      tokenize "/bob/ross/g" `shouldBe` [TokRegexp "bob",  TokRegexp "ross", TokChar 'g']
      tokenize "s/bob/ross/g" `shouldBe` [TokChar 's', TokRegexp "bob",  TokRegexp "ross", TokChar 'g']
    it "finds the correct line" $
      findRegexTarget "3" dummyState `shouldBe` Right (Line 2, Line 2)
    it "searches forward from current line" $
      findRegexTarget "r" dummyState2 `shouldBe` Right (Line 3, Line 3)
    it "Looks forward" $
      findRegexTarget "o" dummyState2 `shouldBe` Right (Line 3, Line 3)
    it "Looks backwards" $
      findRegexTarget "One" dummyState2 `shouldBe` Right (Line 0, Line 0)
    it "Updates the last regex search" $
      ee "/3/" dummyState `shouldBe` Right (Command (Line 2, Line 2) Goto [], dummyState {lastRegex = "3"})
    it "Reuses the last regex" $
      ee "//p" dummyState {lastRegex = "3"} `shouldBe` Right (Command (Line 2, Line 2) Print [], dummyState { lastRegex = "3"})
    it "parses substitute command" $
      baseParser (tokenize "s/3/4/g") dummyState `shouldBe` Right (Command {target = (Line 2, Line 2), op=Substitute, params = [TokRegexp "3", TokRegexp "4", TokChar 'g']}, dummyState {lastRegex="3"})

  describe "Test substitution" $ do
    it "Parses backreferences" $
      head (tokenizeRegexReplacement "\\1234bob") `shouldBe` STokBackref 1234
  describe "Substitution eval" $ do
    it "Replaces" $ do
      (
        readRegexReplacement (TokRegexp "(x)bob([0-9]+)") [(STokBackref 0), (STokString " STR "), (STokBackref 1), (STokBackref 0)] (T.pack "xbob123 hello")
       `shouldBe` "x STR 123x"
       )
    it "parses" $ do
      ee "s/l(.)/xxxx\\0\\0\\0\\0xxxx/" dSt `shouldBe` Right (Command {target = (Line 1,Line 1), op = Substitute, params = [TokRegexp "l(.)",TokRegexp "xxxx\\0\\0\\0\\0xxxx"]},State {buffer = ["l1","l2","l3","l4"], position = 0, registers = fromList [('a',Line 1),('b',Line 2),('p',Line 2)], mode = NormalMode, lastRegex = "l(.)"})
    it "replaces second" $ do
       evaluate (Command {target = (Line 1,Line 1), op = Substitute, params = [TokRegexp "l(.)",TokRegexp "xxxx\\0\\0\\0\\0xxxx"]}) cmdState `shouldBe` (cmdState, "xxxx2222xxxx")
       where cmdState =  (State {buffer = ["l1","l2","l3","l4"], position = 0, registers = fromList [('a',Line 1),('b',Line 2),('p',Line 2)], mode = NormalMode, lastRegex = "l(.)"})
