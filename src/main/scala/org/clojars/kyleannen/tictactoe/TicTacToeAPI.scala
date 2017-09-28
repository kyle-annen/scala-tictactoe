package org.clojars.kyleannen.tictactoe

object TicTacToeAPI {

  def startGame(): GameState = {
    new GameState(
      Board.initBoard(9),
      gameOver = false,
      messages = List(
        Dialog.lang("EN")("greeting"),
        Dialog.lang("EN")("inputPrompt")
      ),
      humanMove = -1,
      computerMove = -1,
      humanToken = "X",
      computerToken = "O",
      gameOutcome = "none",
      gameWinner = "none",
      validSubmission = true,
      language = "EN")
  }

  def playRound(gameState: GameState): GameState = {
    gameState.progressGameState()
  }


}
