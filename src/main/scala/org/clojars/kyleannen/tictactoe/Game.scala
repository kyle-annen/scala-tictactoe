package org.clojars.kyleannen.tictactoe

import scala.annotation.tailrec

import TTTable.TranspositionTable

object Game {
  def setup(
             currentPlayer: Int,
             output: String => Any,
             leftPadding: Int,
             whiteSpace: Int,
             getInput: Int => String,
             loopCount: Int,
             dialogLang: Map[String, String]): Map[Int, Boolean] = {

    View.renderWhitespace(output, whiteSpace)

    val player1 = GameSetup.setPlayer(output, leftPadding, getInput, dialogLang, 1, "X")
    val player2 = GameSetup.setPlayer(output, leftPadding, getInput, dialogLang, 2, "O")

    val players = List(player1, player2).flatten.toMap
    val boardDimension: Int = GameSetup.setBoardSize(output, leftPadding, getInput, dialogLang)
    val boardSize: Int = boardDimension * boardDimension
    val board = Board.initBoard(boardSize)

    val ttTable = new TranspositionTable

    go(board, players, dialogLang, gameOver = false, currentPlayer,
      output, leftPadding, whiteSpace, getInput, loopCount, ttTable)
  }

  @tailrec def go(
  board: List[String],
  players: Map[Int, (String, String, Int)],
  dialogLang: Map[String, String],
  gameOver: Boolean,
  currentPlayer: Int,
  output: String => Any,
  leftPadding: Int,
  whiteSpace: Int,
  getInput: Int => String,
  loopCount: Int,
  ttTable: TranspositionTable): Map[Int, Boolean] = {

    View.renderWhitespace(output, whiteSpace)

    val fBoard = View.formatBoard(board)
    View.renderBoard(output, fBoard, leftPadding)

    val playerNumAnnounce: String = dialogLang("playerAnnounce") + currentPlayer
    View.renderDialog(output, leftPadding, playerNumAnnounce)

    val validPlays: List[String] = Board.returnValidInputs(board)
    val inputPrompt: String = dialogLang("inputPrompt")
    val invalidPlay: String = dialogLang("invalidPlay")
    val playerType: String = players(currentPlayer)._1
    val userToken: String = players(currentPlayer)._2
    val playerDifficulty: Int = players(currentPlayer)._3
    val difficulty: Int = GameSetup.getDifficulty(board, playerDifficulty)
    val oppToken: String = if(userToken == "X") "O" else "X"
    //get the move value
    val boardMove = if(playerType == "human") {
      //human move
      val humanPlay: String = IO.getValidMove(
      validPlays,
      inputPrompt,
      invalidPlay,
      output,
      getInput,
      leftPadding,
      loopCount)
      humanPlay
    } else {
      //AI computer move
      val compMove = AI.negaMax(
      boardState = board,
      nodeMap = Map(0 -> Map()),
      depth = 0,
      maxToken = userToken,
      minToken = oppToken,
      currentToken = userToken,
      depthLimit = difficulty)
      compMove
    }

    val updatedBoard: List[String] = board.map(
    x => if (x == boardMove.toString) userToken else x
    )

    val gameOver: Boolean = Board.gameOver(updatedBoard)

    if(gameOver) {
      View.renderWhitespace(output, whiteSpace)
      val endBoard: List[List[String]] = View.formatBoard(updatedBoard)
      View.renderBoard(output, endBoard, leftPadding)
      View.renderDialog(output, leftPadding, dialogLang("gameOver"))
      val isWin: Boolean = Board.checkWin(updatedBoard)

      if (isWin) {
        View.renderDialog(output, leftPadding, playerNumAnnounce, dialogLang("win"))
      } else {
        View.renderDialog(output, leftPadding, dialogLang("tie"))
      }

      View.renderWhitespace(output, 5)
      //this is the return value of the game
      Map(currentPlayer -> isWin)
    } else {
      val nextPlayer: Int = if(currentPlayer == 1) 2 else 1
      val newLoopCount = loopCount + 1

      go(
      updatedBoard, players, dialogLang, gameOver = false, nextPlayer, output,
      leftPadding, whiteSpace, getInput, newLoopCount, ttTable)
    }
  }


  @tailrec def continueGameLoop(
                         output: String => Any,
                         getInput: Int => String,
                         langSelected: String): Map[Int, Boolean] = {

    View.renderWhitespace(output, 100)

    val selectedLanguage = if (langSelected == "none") {
      GameSetup.setLanguage(output, 15, getInput)
    } else {
      langSelected
    }

    val dialogLang = Dialog.lang(selectedLanguage)
    val gameOutcome = setup(1, output, 15, 100, getInput, 1, dialogLang)

    val continuePlaying = IO.getValidMove(
      List("y", "n"),
      dialogLang("continuePlaying"),
      dialogLang("invalidPlay"),
      output,
      getInput,
      15,
      1)
    //recursive call
    val contBoolean = if (continuePlaying == "y") true else false
    if(contBoolean) {
      continueGameLoop(output, getInput, selectedLanguage)
    } else {
      gameOutcome
    }
  }

  def main(args: Array[String]): Unit = {
    continueGameLoop(println, IO.getInput, "none")
  }
}
