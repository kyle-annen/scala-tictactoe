package org.clojars.kyleannen.tictactoe

import org.scalatest._

class GameSpec extends FunSpec {
  def testPrint(s: String): Unit = return

  def mockInput(callCount: Int): String = {
    callCount match {
      case 1 => "0"
      case 2 => "2"
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

  val mockPlayers: Map[Int, (String, String, Int)] = Map(1 -> ("human", "X",3), 2 -> ("human", "O", 3))





  describe("go") {
    it("the game will finish if there is a winner") {
      val testBoard = Board.initBoard(9)
      val testPlayers = mockPlayers
      val ttTable = new TTTable.TranspositionTable
      val actual = Game.go(
        board = testBoard,
        players = testPlayers,
        dialogLang = Dialog.lang("EN"),
        gameOver = false,
        currentPlayer = 1,
        output = testPrint,
        leftPadding = 0,
        whiteSpace = 0,
        getInput = mockInput9RoundsFinish,
        loopCount = 1,
        ttTable = ttTable).keys.head

      assert(actual == 1 || actual == 2)
    }

    it("the game will end if there is a tie") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
      val ttTable = new TTTable.TranspositionTable
      val actual = Game.go(
        testBoard,
        players = testPlayers,
        dialogLang = Dialog.lang("EN"),
        gameOver = false,
        currentPlayer = 1,
        output = testPrint,
        leftPadding = 0,
        whiteSpace = 0,
        getInput = mockInput9RoundsTie,
        loopCount = 1,
        ttTable = ttTable).keys.head

      assert(actual == 1 || actual == 2)
    }

    it("the game will end early if there is a winner") {
      val testBoard = (1 to 9).toList.map(x => x.toString)
      val testPlayers = mockPlayers
      val ttTable = new TTTable.TranspositionTable
      val actual = Game.go(
        testBoard,
        testPlayers,
        dialogLang = Dialog.lang("EN"),
        gameOver = false,
        currentPlayer = 1,
        output = testPrint,
        leftPadding = 0,
        whiteSpace = 0,
        getInput = mockInput5RoundsFinish,
        loopCount = 1,
        ttTable = ttTable).keys.head

      assert(actual == 1 || actual == 2)
    }
  }

  describe("setup") {

    def mockInputChinese9round(callCount: Int): String = {
      callCount match {
        case 1 => "2"
        case 2 => "3"
        case 3 => "2"
        case 4 => "3"
        case 5 => "3"
      }
    }

    it("language can be chosen and the game played") {
      val actual = Game.setup(
        currentPlayer = 1,
        output = testPrint,
        leftPadding = 0,
        whiteSpace = 0,
        getInput = mockInputChinese9round,
        loopCount = 1,
        dialogLang = Dialog.lang("CN")).keys.head

      assert(actual == 1 || actual == 2)
    }
  }

  describe("contLoop") {
    it("allows the game quit") {
      def mockContTestInput(callCount: Int): String = {
        callCount match {
          case 1 => "2"
          case 2 => "2"
          case 3 => "2"
          case 4 => "3"
          case 5 => "n"
        }
      }
      val actual = Game.continueGameLoop(testPrint, mockContTestInput, "none").keys.head
      assert(actual == 1 || actual == 2)
    }
  }
}
