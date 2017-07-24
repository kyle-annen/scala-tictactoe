package tictactoe

import scala.annotation.tailrec

object AI {

  class AlphaBeta {
    var alpha: Double = Double.NegativeInfinity
    var beta: Double = Double.PositiveInfinity
  }

  class Score(p: Int, v: Int, o: String) {
    val position: Int = p
    val value: Int = v
    val outcome: String = o
  }

  def setDepthLimit(
    boardSize: Int,
    difficulty: String,
    depth: Int): Int = {
    boardSize match {
      case 9 => difficulty match {
        case "easy" => 1
        case "medium" => 3
        case "hard" => 10
      }
      case 16 => depth match {
        case x if(x < 3) => difficulty match {
          case "easy" => 1
          case "medium" => 3
          case "hard" => 6
        }
        case x if(x < 5) => difficulty match {
          case "easy" => 1
          case "medium" => 3
          case "hard" => 10
        }
        case x if(x >= 7) => difficulty match {
          case "easy" => 1
          case "medium" => 3
          case "hard" => 16
        }
      }
      case 25 => difficulty match {
        case "easy" => 1
        case "medium" => 3
        case "hard" => 5
      }
      case _ => difficulty match {
        case "easy" => 1
        case "medium" => 3
        case "hard" => 4
      }
    }
  }

  class AIParams(
    board: List[String],
    currDepth: Int,
    maxT: String,
    minT: String,
    curT: String,
    transTable: TTTable.TranspositionTable,
    diff: String) {

    val boardState: List[String] = board
    val currentDepth: Int = currDepth
    val maxToken: String = maxT
    val minToken: String = minT
    val currentToken: String = curT
    val ttTable: TTTable.TranspositionTable = transTable
    val difficulty: String = diff
    val boardSize: Int = boardState.length
  }

  def getOpenMoves(board: List[String]): List[Int] = {
    Board.returnValidInputs(board).map(x => x.toInt)
  }

  def depthLimitScore(aiParams: AIParams, moveLocation: Int, depthLimit: Int): Score = {
    val score = if(aiParams.maxToken == aiParams.currentToken) {
      1000 - aiParams.currentDepth - 2
    } else {
      -1000 + aiParams.currentDepth + 2
    }
    new Score(moveLocation, score, "depthLimit")
  }

  def updateBoard(board: List[String], token: String, move: Int): List[String] = {
    board.map(x=> if(x == (move).toString) token else x)
  }

  def getComputerMove(compAiParams: AIParams): Score = {
    val alphaBeta = new AlphaBeta
    val depthLimit: Int = setDepthLimit(
        compAiParams.boardSize,
        compAiParams.difficulty,
        compAiParams.currentDepth)

    def miniMax(aiParams: AIParams, firstMove: Int): Score  = {
      val openMoves: List[Int] = getOpenMoves(aiParams.boardState)
      val scores = openMoves.map { move =>
        val initialMove = if(firstMove == 0) move else firstMove.toInt

        if(aiParams.currentDepth >= depthLimit) {
          depthLimitScore(aiParams, initialMove, depthLimit)
        } else if(aiParams.maxToken == aiParams.currentToken){
          val maxScore = 1000 - aiParams.currentDepth
          val maxBoard = updateBoard(aiParams.boardState, aiParams.currentToken, move)
          val maxWin = Board.checkWin(maxBoard)
          val maxTie = Board.checkTie(maxBoard)
          if(maxWin) {
            new Score(initialMove, maxScore, "win")
          } else if(maxTie) {
            new Score(initialMove, 0, "tie")
          } else {
            val maxParams = new AIParams(
            maxBoard,
            aiParams.currentDepth + 1,
            aiParams.maxToken,
            aiParams.minToken,
            aiParams.minToken,
            aiParams.ttTable,
            aiParams.difficulty)
            //recursive call
            miniMax(maxParams, initialMove)
          }
        } else {
          val minScore = -1000 + aiParams.currentDepth
          val minBoard = updateBoard(aiParams.boardState, aiParams.currentToken, move)
          val minWin = Board.checkWin(minBoard)
          val minTie = Board.checkTie(minBoard)
          if(minWin) {
            new Score(initialMove, minScore, "win")
          } else if(minTie) {
            new Score(initialMove, 0, "tie")
          } else {
            val minParams = new AIParams(
              minBoard,
              aiParams.currentDepth + 1,
              aiParams.maxToken,
              aiParams.minToken,
              aiParams.maxToken,
              aiParams.ttTable,
              aiParams.difficulty)
              //recursive call
            miniMax(minParams, initialMove)
          }
        }
      }
      //max path
      if(aiParams.currentToken == aiParams.maxToken) {
        scores.maxBy(_.value)
      } else {
        scores.minBy(_.value)
      }
    }
    miniMax(compAiParams, 0)
  }
}
