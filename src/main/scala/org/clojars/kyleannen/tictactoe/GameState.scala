package org.clojars.kyleannen.tictactoe

import org.clojars.kyleannen.tictactoe.AI.{NodeMap, negaMax}

import scala.collection.mutable.ListBuffer

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
  val validSubmission: Boolean,
  val language: String) {

  def getLocationValue(position: Int): String = {
    board(position - 1)
  }

  def placeHumanMove(): GameState = {
    if(!validSubmission) return this
    if(gameOver) return this
    val board: List[String] = this.board
    val updatedBoard: List[String] = board.map(
      x => if (x == this.humanMove.toString) this.humanToken else x
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
      this.validSubmission,
      this.language)
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
      this.validSubmission,
      this.language)
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
      this.validSubmission,
      this.language)
  }

  def validateGameState(): GameState = {
    if(!this.validSubmission) return this
    val validSubmission = validHumanMove

     new GameState(
      this.board,
      this.gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      validSubmission,
      this.language)
  }

  def validHumanMove: Boolean = {
    Board.returnValidInputs(board).contains(humanMove.toString)
  }


  def checkGameOver(): GameState = {
    if(!validSubmission) return this
    val gameOver = Board.gameOver(board)

    new GameState(
      this.board,
      gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      this.validSubmission,
      this.language)
  }

  def checkTie(): GameState = {
    if(!validSubmission) return this
    val gameOutcome = if(Board.checkTie(this.board)) "tie" else this.gameOutcome

    new GameState(
      this.board,
      this.gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      gameOutcome,
      this.gameWinner,
      this.validSubmission,
      this.language)
  }

  def checkWinner(): GameState = {
    if(!validSubmission) return this
    val gameWinner = Board.getWinner(board)
    val gameOutcome =
      if(gameWinner == "none") this.gameOutcome else "win"

    new GameState(
      this.board,
      this.gameOver,
      this.messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      gameOutcome,
      gameWinner,
      this.validSubmission,
      this.language)
  }

  def addMessages(): GameState = {
    val dialog = Dialog.lang(this.language)
    var messageBuffer = new ListBuffer[String]

    if(this.gameOver) messageBuffer += dialog("gameOver")
    if(this.gameOutcome == "tie") messageBuffer += dialog("tie")
    if(this.gameOutcome == "win") {
      messageBuffer += dialog("playerAnnounce") + " " + this.gameWinner + ", " + dialog("win")
    }
    if(this.gameOutcome == "none") messageBuffer += dialog("inputPrompt")
    if(!this.validSubmission) messageBuffer += dialog("invalidPlay")
    val messages = messageBuffer.toList

    new GameState(
      this.board,
      this.gameOver,
      messages,
      this.humanMove,
      this.computerMove,
      this.humanToken,
      this.computerToken,
      this.gameOutcome,
      this.gameWinner,
      this.validSubmission,
      this.language)
  }

  def progressGameState(): GameState = {
    this.validateGameState()
      .checkGameOver()
      .placeHumanMove()
      .checkGameOver()
      .setComputerMove()
      .placeComputerMove()
      .checkGameOver()
      .checkWinner()
      .checkTie()
      .addMessages()
  }
}
