package org.clojars.kyleannen.tictactoe

import scala.annotation.tailrec

object Game {

  def setLanguage(
                   output: String => Any,
                   leftPadding: Int,
                   getInput: Int => String): String = {

    View.renderDialog(output, leftPadding, Dialog.lang("EN")("greeting"))
    View.renderWhitespace(output, 2)
    val langOptions = ( 1 to Dialog.lang.keys.size).toList.map(x => x.toString)
    val langKeys = Dialog.lang.keys.toList
    for(option <- langOptions) {
      View.renderDialog(output, leftPadding, option + " - " + langKeys(option.toInt - 1))
    }

    val langSelection = IO.getValidMove(
      langOptions,
      Dialog.lang("EN")("selectLang"),
      Dialog.lang("EN")("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    val langKey = langKeys(langSelection.toInt - 1)
    langKey
  }

  def setBoardSize(
                    output: String => Any,
                    leftPadding: Int,
                    getInput: Int => String,
                    dialogLang: Map[String, String]): Int = {

    View.renderDialog(output, leftPadding, dialogLang("pickBoardSize"))
    val sizeOptions = List("3", "4")
    for(option <- sizeOptions) {
      View.renderDialog(output, leftPadding, option + " - " + option + "x" + option)
    }
    val sizeSelection = IO.getValidMove(
      sizeOptions,
      dialogLang("pickBoardSize"),
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    sizeSelection.toInt
  }

  def setDifficulty(
                     output: String => Any,
                     leftPadding: Int,
                     getInput: Int => String,
                     dialogLang: Map[String, String]): Int = {

    val easy = "1 - " + dialogLang("easy")
    val medium = "2 - " + dialogLang("medium")
    val hard = "3 - " + dialogLang("hard")
    val difficultyPrompt = dialogLang("selectDifficulty")

    val prompt = difficultyPrompt +
      "\n" + " " * leftPadding + easy +
      "\n" + " " * leftPadding + medium +
      "\n" + " " * leftPadding + hard
    val validDiffs = List("1","2","3")

    val diffSelection = IO.getValidMove(
      validDiffs,
      prompt,
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)

    diffSelection match  {
      case "1" => 1
      case "2" => 2
      case "3" => 3
    }
  }

  def getDifficulty(board: List[String], difficulty: Int): Int = {
    val openSpacesCount = AI.generateOpenMoves(board).size
    val boardSize = Board.returnDimension(board)
    boardSize match {
      case 3 => {
        difficulty match {
          case 1 => 1
          case 2 => 4
          case 3 => 10
        }
      }
      case 4 => {
        difficulty match {
          case 1 => 1
          case 2 => 2
          case 3 => openSpacesCount match {
            case x if x > 12 => 2
            case x if x <= 12 => 6
          }
        }
      }
    }
  }

  def setPlayer(
                 output: String => Any,
                 leftPadding: Int,
                 getInput: Int => String,
                 dialogLang: Map[String, String],
                 playerNum: Int,
                 playerToken: String): Map[Int, (String, String, Int)] = {

    val pAnnounce = dialogLang("playerAnnounce") + playerNum.toString
    val pPrompt = dialogLang("selectPlayerType")
    val pHuman = dialogLang("pTypeHuman")
    val pComp = dialogLang("pTypeComputer")
    val prompt = pAnnounce + "\n" +  " " * leftPadding +
      pPrompt + "\n" +  " " * leftPadding +
      pHuman + "\n" + " " * leftPadding +
      pComp
    val pTypeOptions: List[String] = List("1","2")
    val playerType = IO.getValidMove(
      pTypeOptions,
      prompt,
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    val pType = if(playerType == "1") "human" else "computer"
    val playerDifficulty = if(pType == "computer") {
      setDifficulty(output, leftPadding, getInput, dialogLang)
    } else {
      1
    }

    val player = Map(playerNum -> (pType, playerToken, playerDifficulty))
    player
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
                   ttTable: TTTable.TranspositionTable): Map[Int, Boolean] = {

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
    val difficulty: Int = getDifficulty(board, playerDifficulty)
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
        board,
        Map(0->Map()),
        0,
        userToken,
        oppToken,
        userToken,
        difficulty)
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
        updatedBoard, players, dialogLang, false, nextPlayer, output,
        leftPadding, whiteSpace, getInput, newLoopCount, ttTable)
    }
  }

  def setup(
             currentPlayer: Int,
             output: String => Any,
             leftPadding: Int,
             whiteSpace: Int,
             getInput: Int => String,
             loopCount: Int,
             dialogLang: Map[String, String]): Map[Int, Boolean] = {

    View.renderWhitespace(output, whiteSpace)

    val player1 = setPlayer(output, leftPadding, getInput, dialogLang, 1, "X")
    val player2 = setPlayer(output, leftPadding, getInput, dialogLang, 2, "O")

    val players = List(player1, player2).flatten.toMap
    val boardDimension: Int = setBoardSize(output, leftPadding, getInput, dialogLang)
    val boardSize: Int = boardDimension * boardDimension
    val board = Board.initBoard(boardSize)

    val ttTable = new TTTable.TranspositionTable

    go(
      board, players, dialogLang, gameOver = false, currentPlayer,
      output, leftPadding, whiteSpace, getInput, loopCount, ttTable)
  }

  @tailrec def contLoop(
                         output: String => Any,
                         getInput: Int => String,
                         langSelected: String): Map[Int, Boolean] = {

    View.renderWhitespace(output, 100)

    val selectedLanguage = if (langSelected == "none") {
      setLanguage(output, 15, getInput)
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
      contLoop(output, getInput, selectedLanguage)
    } else {
      gameOutcome
    }
  }

  def main(args: Array[String]): Unit = {
    contLoop(println, IO.getInput, "none")
  }
}
