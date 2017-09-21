package tictactoe

import org.scalatest.FunSpec

class GameStateSpec extends FunSpec{
  val gameState = new GameState(
    board = Board.initBoard(9),
    gameOver = false,
    messages = List("hi", "hello", "great game"),
    humanMove = 1,
    computerMove = -1,
    humanToken = "X",
    computerToken = "O",
    "none",
    "none",
    true)

  describe("GameState") {
    describe("getBoard") {
      it("returns the board") {
        val actual: List[String] = gameState.board
        val expected: List[String] = Board.initBoard(9)
        assert(expected == actual)
      }
    }

    describe("isGameOver") {
      it("returns game over status") {
        val actual: Boolean = gameState.gameOver
        val expected: Boolean = false
        assert(actual == expected)
      }
    }

    describe("getMessages") {
      it("returns messages") {
        val actual: List[String] = gameState.messages
        val expected: List[String] = List("hi", "hello", "great game")
        assert(actual == expected)
      }
    }

    describe("getHumanMove") {
      it("returns the human move") {
        val actual: Int = gameState.humanMove
        val expected: Int = 1
        assert(actual == expected)
      }
    }

    describe("getComputerMove") {
      it("returns the compute move ") {
        val actual: Int = gameState.computerMove
        val expected: Int = -1
        assert(actual == expected)
      }
    }

    describe("getHumanToken") {
      it("returns the human token") {
        val actual: String = gameState.humanToken
        val expected: String = "X"
        assert(actual == expected)
      }
    }

    describe("getComputerToken") {
      it("return the computer token") {
        val actual: String = gameState.computerToken
        val expected: String = "O"
        assert(actual == expected)
      }
    }

    describe("getLocationValue") {
      it("returns the value of a location on the board") {
        val actual: String = gameState.getLocationValue(1)
        val expected: String = "1"
        assert(expected == actual)
      }
    }

    describe("getGameOutcome") {
      it("returns the game outcome") {
        val actual = gameState.gameOutcome
        val expected = "none"
        assert(actual == expected)
      }
    }

    describe("getGameWinner") {
      it("gets the game winner") {
        val actual = gameState.gameWinner
        val expected = "none"
        assert(actual == expected)
      }
    }

    describe("getValidSubmission") {
      it("gets valid submission value ") {
        val actual = gameState.validSubmission
        val expected = true
        assert(actual == expected)
      }
    }

    describe("placeHumanMove") {
      it("returns and updates board for player move") {
        val testGameState = gameState.placeHumanMove()
        val expected = testGameState.humanToken
        val humanMovePosition = testGameState.humanMove
        val actual = testGameState.getLocationValue(humanMovePosition)
        assert(actual == expected)
      }
    }

    describe("setComputerMove") {
      it("gets a computer move") {
        val testGameState = gameState
          .placeHumanMove()
          .setComputerMove()
        val computerMove = testGameState.computerMove
        assert(computerMove > 0)
      }
    }

    describe("placeComputerMove") {
      it("returns and updates board for computer move") {
        val testGameState = gameState
          .setComputerMove()
          .placeComputerMove()
        val compMoveLocation = testGameState.computerMove
        val compMoveLocationValue = testGameState.getLocationValue(compMoveLocation)
        assert(compMoveLocationValue == testGameState.computerToken)
      }
    }

  }
}
