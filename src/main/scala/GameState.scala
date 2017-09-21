package tictactoe

import tictactoe.AI.NodeMap
import tictactoe.AI.negaMax

class GameState(
               val board: List[String],
               val gameOver: Boolean,
               val messages: List[String],
               val humanMove: Int,
               val computerMove: Int,
               val humanToken: String,
               val computerToken: String,
               val gameOutcome: String,
               val gameWinner: String,
               val validSubmission: Boolean) {

  def getLocationValue(position: Int): String = {
    board(position - 1)
  }

  def placeHumanMove(): GameState = {
    if(!validSubmission) return this
    if(gameOver) return this
    val board: List[String] = this.board
    val boardMove: Int = this.humanMove
    val updatedBoard: List[String] = board.map(
      x => if (x == boardMove.toString) this.humanToken else x
    )
    new GameState(
      updatedBoard,
      this.gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      this.validSubmission)
  }

  def placeComputerMove(): GameState = {
    if(!validSubmission) return this
    if(gameOver) return this
    val board: List[String] = this.board
    val boardMove: Int = this.computerMove
    val updatedBoard: List[String] = board.map(
      x => if (x == boardMove.toString) this.computerToken else x
    )
    new GameState(
      updatedBoard,
      this.gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      this.validSubmission)
  }

  def setComputerMove(): GameState = {
    if(!validSubmission) return this
    if(gameOver) return this
    val boardState: List[String] = this.board
    val startNodeMap: NodeMap = Map(0->Map())
    val startDepth: Int = 0
    val maxToken: String = this.computerToken
    val minToken: String = this.humanToken
    val currentToken: String = this.computerToken
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
      this.board,
      this.gameOver,
      this.messages,
      this.humanMove,
      computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      this.validSubmission)
  }

  def validateGameState(): GameState = this

  def addMessages(): GameState = this

  def progressGameState(): GameState = {
    this.validateGameState()
      .placeHumanMove()
      .setComputerMove()
      .placeComputerMove()
  }
}
