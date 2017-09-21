package tictactoe

object TicTacToeAPI {

  def updateGame(gameState: GameState): GameState = {
    gameState
      .validateGameState()
      .placeHumanMove()
      .setComputerMove()
      .placeComputerMove()
  }

  def validateGameState(gameState: GameState): GameState = {
    gameState
  }


}


