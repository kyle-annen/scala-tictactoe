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

  def setDepthLimit(boardSize: Int, difficulty: String, depth: Int): Int = {
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

  def getComputerMove(
    origBoardState: List[String],
    maxPlayerToken: String,
    minPlayerToken: String,
    currentPlayerToken: String,
    ttTable: TTTable.TranspositionTable,
    difficulty: String): Int = {

    val boardSize = origBoardState.length
    val alphaBeta = new AlphaBeta


    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {

      val depthLimit = setDepthLimit(boardSize, difficulty, depth)
      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)

      val scores = openMoves.map(move =>
        //maxpath
        if(depth >= depthLimit) {
          val depthLimitScore = if(curT == maxT) -1000 + depth + 2 else 1000 - depth - 2
          new Score(move, depthLimitScore, "depthLimit")
        } else if(curT == maxT) {
          val maxScore = 1000 - depth
          val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val maxWin = Board.checkWin(maxBoardMove)
          val maxTie = Board.checkTie(maxBoardMove)

          if(maxWin) {
            new Score(move, maxScore, "win")
          } else if(maxTie) {
            new Score(move, 0, "tie")
          } else {
            val mmResult = miniMax(maxBoardMove, depth + 1, maxT, minT, minT)
            val mmScore = mmResult(mmResult.keys.head)
            Map(move -> mmScore)
            }
          }
        //min path
        } else {
          val minScore = -1000 + depth
          val minBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val minWin = Board.checkWin(minBoardMove)
          val minTie = Board.checkTie(minBoardMove)
          if(minWin) {
            new Score(move, minScore, "win")
          } else if(minTie) {
            new Score(move, 0, "tie")
          } else {
            val mmResult = miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
            val mmScore = mmResult(mmResult.keys.head)
            Map(move -> mmScore)
          }
        }
      )

      val mapScores = scores.flatten.toMap


      if(curT == maxT) {
        val v = -1001
        val maxTupleScore = mapScores.maxBy(_._2)
        val maxMapScore = Map(maxTupleScore._1 -> maxTupleScore._2)
        alphaBeta.alpha = List(maxTupleScore._2, alphaBeta.alpha.toInt, v).max
        if(alphaBeta.beta <= alphaBeta.alpha) {
          Map(maxTupleScore._1 -> v)
        } else {
          maxMapScore
        }
      } else {
        val v = 1001
        val minTupleScore = mapScores.minBy(_._2)
        val minMapScore = Map(minTupleScore._1 -> minTupleScore._2)
        alphaBeta.beta = List(minTupleScore._2, alphaBeta.beta.toInt, v).max
        if(alphaBeta.beta <= alphaBeta.alpha) {
          new Score()
          Map(minTupleScore._1 -> v)
        } else {
          minMapScore
        }
      }
    }
    //call the recursive function
    val result = miniMax(origBoardState, 1, maxPlayerToken, minPlayerToken, maxPlayerToken)
    result.keys.head
  }
}
// val board = List("X","O","X","X","O","X","7","8","9")
