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

  def mockInput9RoundsTie(callCount: Int): String = {
    callCount match {
      case 1 => "1"
      case 2 => "2"
      case 3 => "3"
      case 4 => "7"
      case 5 => "8"
      case 6 => "9"
      case 7 => "4"
      case 8 => "5"
      case 9 => "6"
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

  def mockInput5RoundsFinish(callCount: Int): String = {
    callCount match {
      case 1 => "1"
      case 2 => "2"
      case 3 => "4"
      case 4 => "3"
      case 5 => "7"
    }
  }

  val mockPlayers: Map[Int, (String, String)] = Map(1 -> ("human", "X"), 2 -> ("human", "O"))

  describe("setLanguage") {
    it("sets the language of the game") {
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
    it("the game will finish if there is a winner") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
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

    it("the game will end if there is a tie") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
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
        mockInput9RoundsTie,
        1)

      assert(actual == expected)
    }

    it("the game will end early if there is a winner") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
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
        mockInput5RoundsFinish,
        1)

      assert(actual == expected)
    }
  }

  describe("setup") {

    def mockInputEnglish9round(callCount: Int): String = {
      callCount match {
        case 1 => "1"
        case 2 => "1"
        case 3 => "1"
        case 4 => "1"
        case 5 => "1"
        case 6 => "4"
        case 7 => "2"
        case 8 => "6"
        case 9 => "3"
        case 10 => "9"
      }
    }

    it("English can be chosen and game played") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
      val expected = true
      val actual = Game.setup(
        testBoard,
        1,
        testPrint,
        0,
        0,
        mockInputEnglish9round,
        1)

      assert(actual == expected)
    }

    def mockInputChinese9round(callCount: Int): String = {
      callCount match {
        case 1 => "1"
        case 2 => "1"
        case 3 => "1"
        case 4 => "1"
        case 5 => "7"
        case 6 => "2"
        case 7 => "5"
        case 8 => "3"
        case 9 => "8"
        case 10 => "9"
      }
    }

    it("Chinese can be chosen and the game played") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
      val expected = true
      val actual = Game.setup(
        testBoard,
        1,
        testPrint,
        0,
        0,
        mockInputChinese9round,
        1)

      assert(actual == expected)
    }
  }
}
