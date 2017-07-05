package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class GameSpec extends FunSpec {

  def testPrint(s: String): Unit = return
  
  def mockInput(callCount: Int): String = {
    callCount match {
      case 0 => "en"
      case 1 => "CN"
    }
  }

  def mockInput9RoundsFinish(callCount: Int): String = {
    callCount match {
      case 1 => "1"
      case 2 => "2"
      case 3 => "3"
      case 4 => "4"
      case 5 => "5"
      case 6 => "6"
      case 7 => "7"
      case 8 => "8"
      case 9 => "9"
    }
  }
  describe("initPlayers") {
    it("initializes players") {
      val expected = Map(1 -> "X", 2 -> "O")
      val actual = Game.initPlayers
      assert(actual == expected)
    }
  }

  describe("setLanguage") {
    it("set the language of the game") {
      val expected = "CN"
      val actual = 
        Game.setLanguage(
          testPrint,
          3,
          mockInput)
      assert(actual == expected)
    }
  }

  describe("go") {
    it("be able to be played") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = Game.initPlayers
      val expected = true
      val actual = Game.go(
        testBoard,
        testPlayers,
        Dialog.lang("EN"),
        false,
        1,
        testPrint,
        0,
        0,
        mockInput9RoundsFinish,
        1)

      assert(actual == expected)
    }
  }
}