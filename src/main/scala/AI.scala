package tictactoe

import scala.annotation.tailrec

object AI {

  class AlphaBeta {
    var alpha: Double = Double.NegativeInfinity
    var beta: Double = Double.PositiveInfinity
  }

  def setDepthLimit(boardSize: Int, difficulty: String): Int = {
    boardSize match {
      case 9 => difficulty match {
        case "easy" => 1
        case "medium" => 3
        case "hard" => 100
      }
      case 16 => difficulty match {
        case "easy" => 1
        case "medium" => 3
        case "hard" => 6
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

    val depthLimit = setDepthLimit(boardSize, difficulty)

    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {

      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)

      val scores = openMoves.map(move =>
        //maxpath
        if(depth >= depthLimit) {
          val depthLimitScore = if(curT == maxT) -1000 + depth + 2 else 1000 - depth - 2
          Map(move -> depthLimitScore)
        } else if(curT == maxT) {
          val maxScore = 1000 - depth
          val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val maxWin = Board.checkWin(maxBoardMove)
          val maxTie = Board.checkTie(maxBoardMove)
          if(maxWin) {
/*            val maxTranspositions = getBoardTranspositions(maxBoardMove, maxScore, maxT, minT)*/
            /*saveTranspositions(ttTable, maxTranspositions, "max")*/
            Map(move -> maxScore)
          } else if(maxTie) {
  /*          val maxTieTranspositions = getBoardTranspositions(maxBoardMove, 0, maxT, minT)*/
            /*saveTranspositions(ttTable, maxTieTranspositions, "max")*/
            Map(move -> 0)
          } else {
            val checkTransTable = TTTable.checkTransposition(maxBoardMove, ttTable, maxT, minT, "max")
            if(checkTransTable._1 == true) {
              Map(move -> checkTransTable._2)
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
            /*val minTranspositions = getBoardTranspositions(minBoardMove, minScore, maxT, minT)*/
            /*saveTranspositions(ttTable, minTranspositions, "min")*/
            Map(move -> minScore)
          } else if(minTie) {
           /* val minTieTranspostitions = getBoardTranspositions(minBoardMove, 0, maxT, minT)*/
            /*saveTranspositions(ttTable, minTieTranspostitions, "min")*/
            Map(move -> 0)
          } else {
            val checkTransTable = TTTable.checkTransposition(minBoardMove, ttTable, maxT, minT, "min")
            if(checkTransTable._1 == true) {
              Map(move -> checkTransTable._2)
            } else {
              val mmResult = miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
              val mmScore = mmResult(mmResult.keys.head)
              Map(move -> mmScore)
            }
          }
        }
      )

      val mapScores = scores.flatten.toMap

      //transposition table saving
     /* val boardScores = mapScores.map { pair =>*/
        //(
          //currentBoard.map { cell =>
            //if (cell == pair._1.toString) {
              //curT
            //} else {
              //cell
            //}
          //},
          //pair._2
        //)
      //}

      //if(curT == maxT) {
        //boardScores.map { boardScore =>
          //val maxTranspositions = getBoardTranspositions(boardScore._1, boardScore._2, maxT, minT)
          //saveTranspositions(ttTable, maxTranspositions, "max")
        //}
      //} else {
        //boardScores.map { boardScore =>
          //val minTranspositions = getBoardTranspositions(boardScore._1, boardScore._2, maxT, minT)
          //saveTranspositions(ttTable, minTranspositions, "min")
        //}
      /*}*/


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
