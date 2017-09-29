package org.clojars.kyleannen.tictactoe

import org.scalatest.FunSpec

class GameStateSpec extends FunSpec{
class GameStateSpec extends FunSpec{
  val gameState = new GameState(
    board = Board.initBoard(9),
    gameOver = false,
    messages = List("hi", "hello", "great game"),
    humanMove = 1,
    computerMove = -1,
    humanToken = "X",
    computerToken = "O",
    gameOutcome = "none",
    gameWinner = "none",
    validSubmission = true,
    language = "EN")

  describe("GameState") {
    describe("board") {
      it("returns the board") {
        val actual: List[String] = gameState.board
        val expected: List[String] = Board.initBoard(9)
        assert(expected == actual)
      }
    }

    describe("gameOver") {
      it("returns game over status") {
        val actual: Boolean = gameState.gameOver
        val expected: Boolean = false
        assert(actual == expected)
      }
    }

    describe("messages") {
      it("returns messages") {
        val actual: List[String] = gameState.messages
        val expected: List[String] = List("hi", "hello", "great game")
        assert(actual == expected)
      }
    }

    describe("humanMove") {
      it("returns the human move") {
        val actual: Int = gameState.humanMove
        val expected: Int = 1
        assert(actual == expected)
      }
    }

    describe("computerMove") {
      it("returns the compute move ") {
        val actual: Int = gameState.computerMove
        val expected: Int = -1
        assert(actual == expected)
      }
    }

    describe("humanToken") {
      it("returns the human token") {
        val actual: String = gameState.humanToken
        val expected: String = "X"
        assert(actual == expected)
      }
    }

    describe("computerToken") {
      it("return the computer token") {
        val actual: String = gameState.computerToken
        val expected: String = "O"
        assert(actual == expected)
      }
    }

    describe("locationValue") {
      it("returns the value of a location on the board") {
        val actual: String = gameState.getLocationValue(1)
        val expected: String = "1"
        assert(expected == actual)
      }
    }

    describe("gameOutcome") {
      it("returns the game outcome") {
        val actual = gameState.gameOutcome
        val expected = "none"
        assert(actual == expected)
      }
    }

    describe("gameWinner") {
      it("gets the game winner") {
        val actual = gameState.gameWinner
        val expected = "none"
        assert(actual == expected)
      }
    }

    describe("validSubmission") {
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

    describe("validHumanMove") {
      it("returns true if human move is valid") {
        assert(gameState.validHumanMove)
      }

      it("returns false if human move is invalid") {
        val testGameState = new GameState(
          board = Board.initBoard(9),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 0,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        assert(!testGameState.validHumanMove)
      }
    }

    describe("checkGameOver") {
      it("sets game over when game is over") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","O"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 8,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState = testGameState
          .placeHumanMove()
          .checkGameOver()

        val expectedGameOver = true
        val actualGameOver = updatedGameState.gameOver
        assert(actualGameOver == expectedGameOver)
      }

      it("sets the game over if there is a win") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "O","O","X",
            "O","8","9"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 9,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState = testGameState
          .placeHumanMove()
          .checkGameOver()

        val expectedGameOver = true
        val actualGameOver = updatedGameState.gameOver
        assert(actualGameOver == expectedGameOver)
      }
    }

    describe("checkWinner") {
      it("sets the winner and updates game outcome on win") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "O","O","X",
            "O","8","9"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 9,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState = testGameState
          .placeHumanMove()
          .checkWinner()

        val expectedGameOutcome = "win"
        val actualGameOutcome = updatedGameState.gameOutcome
        assert(expectedGameOutcome == actualGameOutcome)

        val expectedWinner = "X"
        val actualWinner = updatedGameState.gameWinner
        assert(expectedWinner == actualWinner)
      }
    }

    describe("checkTie") {
      it("sets a tie if there is a tie") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","O"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 8,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState =
          testGameState.placeHumanMove().checkTie()

        val expectedGameOutcome: String = "tie"
        val actualGameOutcome = updatedGameState.gameOutcome
        assert(expectedGameOutcome == actualGameOutcome)
      }
    }

    describe("addMessages") {
      it("prompts next turn ") {
        val testGameState = gameState.addMessages()
        val expected = Dialog.lang(gameState.language)("inputPrompt")
        assert(testGameState.messages.contains(expected))
      }

      it("has gameOver message if game is over") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","O"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 8,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState =
          testGameState
            .placeHumanMove()
            .checkGameOver()
            .addMessages()

        val expected = Dialog.lang(testGameState.language)("gameOver")

        assert(updatedGameState.messages.contains(expected))
      }

      it("has tie message if game is tie") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","O"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 8,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState =
          testGameState
            .placeHumanMove()
            .checkGameOver()
            .checkTie()
            .addMessages()

        val expected = Dialog.lang(testGameState.language)("tie")

        assert(updatedGameState.messages.contains(expected))
      }

      it("has input prompt if game is not over") {
        val testGameState = new GameState(
          board = Board.initBoard(9),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 8,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState =
          testGameState
            .placeHumanMove()
            .checkGameOver()
            .setComputerMove()
            .placeComputerMove()
            .checkTie()
            .addMessages()

        val expected = Dialog.lang(testGameState.language)("inputPrompt")

        assert(updatedGameState.messages.contains(expected))
      }


      it("if invalid move is provided has invalid move message") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","O"),
          gameOver = false,
          messages = List(),
          humanMove = 9,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState = testGameState.progressGameState()

        val expected = Dialog.lang(testGameState.language)("invalidPlay")

        assert(updatedGameState.messages.contains(expected))
      }

      it("has announcement of winner if there is a winner") {
        val testGameState = new GameState(
          board = List(
            "X","O","X",
            "X","O","X",
            "O","8","9"),
          gameOver = false,
          messages = List("hi", "hello", "great game"),
          humanMove = 9,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val updatedGameState =
          testGameState.progressGameState()

        val expected = "Player  X, You have won the game!"

        assert(updatedGameState.messages.contains(expected))
      }


    }
  }
}










