package org.clojars.kyleannen.tictactoe

import org.scalatest.FunSpec

class GameSetupSpec extends FunSpec{

  def testPrint(s: String): Unit = return

  def mockInput(callCount: Int): String = {
    callCount match {
      case 1 => "0"
      case 2 => "2"
    }
  }

  describe("setLanguage") {
    it("sets the language of the game") {
      val expected = "CN"
      val actual =
        GameSetup.setLanguage(
          testPrint,
          3,
          mockInput)
      assert(actual == expected)
    }
  }

  describe("setPlayer") {
    it("can select a human player") {
      def mockHumanSelect(callCount: Int): String = {
        callCount match {
          case 1 => "1"
        }
      }

      val expected = Map(1 -> ("human", "X", 1))
      val actual = GameSetup.setPlayer(
        testPrint,
        0,
        mockHumanSelect,
        Dialog.lang("EN"),
        1,
        "X")
      assert(actual == expected)
    }

    it("can select a computer player") {
      def mockHumanSelect(callCount: Int): String = {
        callCount match {
          case 1 => "2"
          case 2 => "3"
        }
      }

      val expected = Map(1 -> ("computer", "X", 2))
      val actual = GameSetup.setPlayer(
        testPrint,
        0,
        mockHumanSelect,
        Dialog.lang("EN"),
        1,
        "X")
      assert(actual == expected)
    }
  }

describe("getDifficulty") {
  describe("3x3 board") {
      val board3x3 = Board.initBoard(9)
      it("returns depth for 3x3 board easy difficulty") {
        val actual = GameSetup.getDifficulty(board3x3, 1)
        val expected = 1
        assert(expected == actual)
      }

      it("returns depth for 3x3 board moderate difficulty") {
        val actual = GameSetup.getDifficulty(board3x3, 2)
        val expected = 4
        assert(expected == actual)
      }

      it("returns depth for 3x3 board hard difficulty") {
        val actual = GameSetup.getDifficulty(board3x3, 3)
        val expected = 10
        assert(expected == actual)
      }
    }
  }

  describe("4x4 board") {
    val board4x4 = Board.initBoard(16)
    it("returns depth for 4x4 board easy difficulty") {
      val actual = GameSetup.getDifficulty(board4x4, 1)
      val expected = 1
      assert(expected == actual)
    }

    it("returns depth for 4x4 board moderate difficulty") {
      val actual = GameSetup.getDifficulty(board4x4, 2)
      val expected = 2
      assert(expected == actual)
    }

    it("returns depth for 4x4 board hard difficulty early game") {
      val testBoard = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16".split(",").toList
      val actual = GameSetup.getDifficulty(testBoard, 3)
      val expected = 2
      assert(expected == actual)
    }

    it("returns depth for 4x4 board hard difficulty late game") {
      val testBoard = "X,X,X,O,O,O,X,O,X,10,11,12,13,14,O,X".split(",").toList
      val actual = GameSetup.getDifficulty(testBoard, 3)
      val expected = 6
      assert(expected == actual)
    }
  }
}
