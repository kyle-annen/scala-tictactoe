package tictactoe

import tictactoe.AI.NodeMap
import tictactoe.AI.negaMax

class GameState(
               board: List[String],
               gameOver: Boolean,
               messages: List[String],
               humanMove: Int,
               computerMove: Int,
               humanToken: String,
               computerToken: String,
               gameOutcome: String,
               gameWinner: String,
               validSubmission: Boolean) {

  def getBoard: List[String] = board

  def isGameOver: Boolean = gameOver

  def getMessages: List[String] = messages

  def getHumanMove: Int = humanMove

  def getComputerMove: Int = computerMove

  def getHumanToken: String = humanToken

  def getComputerToken: String = computerToken

  def getLocationValue(position: Int): String = {
    board(position - 1)
  }

  def getGameOutcome: String = gameOutcome

  def getGameWinner: String = gameWinner

  def getValidSubmission: Boolean = validSubmission

  def placeHumanMove(): GameState = {
    val board: List[String] = this.getBoard
    val boardMove: Int = this.getHumanMove
    val updatedBoard: List[String] = board.map(
      x => if (x == boardMove.toString) this.getHumanToken else x
    )
    new GameState(
      updatedBoard,
      this.isGameOver,
      this.getMessages,
      this.getHumanMove,
      this.getComputerMove,
      this.getHumanToken,
      this.getComputerToken,
      this.getGameOutcome,
      this.getGameWinner,
      this.getValidSubmission)
  }

  def placeComputerMove(): GameState = {
    val board: List[String] = this.getBoard
    val boardMove: Int = this.getComputerMove
    val updatedBoard: List[String] = board.map(
      x => if (x == boardMove.toString) this.getComputerToken else x
    )
    new GameState(
      updatedBoard,
      this.isGameOver,
      this.getMessages,
      this.getHumanMove,
      this.getComputerMove,
      this.getHumanToken,
      this.getComputerToken,
      this.getGameOutcome,
      this.getGameWinner,
      this.getValidSubmission)
  }

  def setComputerMove(): GameState = {
    val boardState: List[String] = this.getBoard
    val startNodeMap: NodeMap = Map(0->Map())
    val startDepth: Int = 0
    val maxToken: String = this.getComputerToken
    val minToken: String = this.getHumanToken
    val currentToken: String = this.getComputerToken
    val depthLimit: Int = 9

    val computerMove = negaMax(
      boardState,
      startNodeMap,
      startDepth,
      maxToken,
      minToken,
      currentToken,
      depthLimit)

    new GameState(
      this.getBoard,
      this.isGameOver,
      this.getMessages,
      this.getHumanMove,
      computerMove,
      this.getHumanToken,
      this.getComputerToken,
      this.getGameOutcome,
      this.getGameWinner,
      this.getValidSubmission)
  }

  def validateGameState(): GameState = this

}
