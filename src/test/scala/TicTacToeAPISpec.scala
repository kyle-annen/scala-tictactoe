package tictactoe

import org.scalatest.FunSpec

class TicTacToeAPISpec extends FunSpec{

  val testGameState: GameState = new GameState(
    board = Board.initBoard(9),
    gameOver = false,
    messages = List(),
    humanMove = 1,
    computerMove = -1,
    humanToken = "X",
    computerToken = "O",
    gameOutcome = "none",
    gameWinner = "none",
    validSubmission = true)

  describe("TicTacToeAPI") {

  }
}
