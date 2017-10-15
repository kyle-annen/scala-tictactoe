package org.clojars.kyleannen.tictactoe

import org.scalatest.FunSpec

class TicTacToeAPISpec extends FunSpec{

  describe("TicTacToeAPI") {
    describe("startGame") {
      it("returns a new gameState") {
        val gameState = TicTacToeAPI.startGame
        assert(gameState.board.length == 9)
        assert(!gameState.board.contains("X"))
        assert(!gameState.board.contains("O"))

        val messages = gameState.messages
        val greeting = Dialog.lang(gameState.language)("greeting")
        val inputPrompt = Dialog.lang(gameState.language)("inputPrompt")

        assert(messages.contains(greeting))
        assert(messages.contains(inputPrompt))
      }
    }

    describe("playRound") {
      it("places the human move") {
        val gameState = new GameState(
          Board.initBoard(9),
          gameOver = false,
          messages = List(""),
          humanMove = 1,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val nextGameState = TicTacToeAPI.playRound(gameState)

        assert(nextGameState.board(0) == "X")
      }

      it("makes a computer move") {
        val gameState = new GameState(
          Board.initBoard(9),
          gameOver = false,
          messages = List(""),
          humanMove = 1,
          computerMove = -1,
          humanToken = "X",
          computerToken = "O",
          gameOutcome = "none",
          gameWinner = "none",
          validSubmission = true,
          language = "EN")

        val nextGameState = TicTacToeAPI.playRound(gameState)

        assert(nextGameState.board.contains("O"))
      }
    }
  }
}
