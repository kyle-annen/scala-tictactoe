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
}
