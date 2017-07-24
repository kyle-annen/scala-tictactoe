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
        case "hard" => 100
      }
      case 16 => depth match {
        case x if(x <= 7) => difficulty match {
          case "easy" => 1
          case "medium" => 3
          case "hard" => 4
        }
        case x if(x > 7)  => difficulty match {
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
    transTable: TTTable.TranspositionTable,
    diff: String) {

    val boardState: List[String] = board
    val currentDepth: Int = currDepth
    val maxToken: String = maxT
    val minToken: String = minT
    val currentToken: String = maxT 
    val ttTable: TTTable.TranspositionTable = transTable
    val difficulty: String = diff
    val boardSize: Int = boardState.length
  }

  def getOpenMoves(board: List[String]): List[Int] = {
    Board.returnValidInputs(board).map(x => x.toInt)
  }

  def depthLimitScore(aiParams: AIParams, moveLocation: Int): Score = {
    val score = if(aiParams.maxToken == aiParams.currentToken) {
      -1000 + aiParams.currentDepth + 2
    } else {
      1000 - aiParams.currentDepth - 2
    }
    new Score(moveLocation, score, "depthLimit")
  }

  def updateBoard(board: List[String], token: String, move: Int): List[String] = {
    board.map(x=> if(x == (move).toString) token else x)
  } 

  def getComputerMove(aiParams: AIParams): Score = {

    val alphaBeta = new AlphaBeta

    def miniMax(aiParams: AIParams): Score  = {
      val depthLimit: Int = setDepthLimit(
        aiParams.boardSize,
        aiParams.difficulty,
        aiParams.currDepth)

      val openMoves: List[Int] = getOpenMoves(aiParams.boardState)
      
      val scores = openMoves.map { move => 
        if(aiParams.currentDepth >= depthLimit) {
          dephtLimitScore(aiParams, move)
        } else if(aiParams.maxToken == aiParams.currentToken){
          val maxScore = 1000 - aiParams.currentDepth
          val maxBoard = updateBoard(aiParams.boardState, aiParams.currentToken, move)
          val maxWin = Board.checkWin(maxBoard)
          val maxTie = Board.checkTie(maxBoard)
          if(maxWin) {
            new Score(move, maxScore, "win")
          } else if(maxTie) {
            new Score(move, 0, "tie")
          } else {
            val maxParams = new AIParams(
              maxBoard,
              aiParams.currentDepth + 1,
              aiParams.maxToken,
              aiParams.minToken,
              aiParams.minToken,
              aiParams.ttTable,
              aiParams.difficulty)
            miniMax(maxParams)
          }
        } else {
          val minScore = 1000 - aiParams.currentDepth
          val minBoard = updateBoard(aiParams.boardState, aiParams.currentToken, move)
          val minWin = Board.checkWin(minBoard)
          val minTie = Board.checkTie(minBoard)
          if(minWin) {
            new Score(move, minScore, "win")
          } else if(minTie) {
            new Score(move, 0, "tie")
          } else {
            val minParams = new AIParams(
              minBoard,
              aiParams.currentDepth + 1,
              aiParams.maxToken,
              aiParams.minToken,
              aiParams.maxToken,
              aiParams.ttTable,
              aiParams.difficulty)
            miniMax(minParams)
          }
        }
      }
      
    }
  }
}