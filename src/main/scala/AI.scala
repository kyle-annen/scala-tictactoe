package tictactoe

import scala.annotation.tailrec

object AI {

  class AlphaBeta {
    var alpha: Double = Double.NegativeInfinity
    var beta: Double = Double.PositiveInfinity
  }

  class TranspositionTable {
    var min = scala.collection.mutable.Map[String, Int]()
    var max = scala.collection.mutable.Map[String, Int]()
  }

  def getBoardTranspositions(
    board: List[String],
    score: Int,
    p1Token: String,
    p2Token: String): List[(String, Int)] = {

    val generalizedBoard = board.map { x =>
      if(x == p1Token) {
        p1Token
      } else if(x == p2Token) {
        p2Token
      } else {
        "-"
      }
    }
    val rowLists = Board.returnRows(generalizedBoard)
    val b1 = rowLists.flatten.foldLeft("")(_ + _)
    val b2 = rowLists.transpose.flatten.foldLeft("")(_ + _)
    val b3 = b1.reverse
    val b4 = b2.reverse

    val transpositions = List(
      (b1 -> score),
      (b2 -> score),
      (b3 -> score),
      (b4 -> score)
    )
    transpositions
  }

  def saveTranspositions(
    tt: TranspositionTable,
    values: List[(String, Int)],
    minOrMax: String): Unit = {
    val path = if(minOrMax == "min") tt.min else tt.max
    for (value <- values) {
      path += value
    }
  }

  def checkTransposition(
    boardState: List[String],
    ttTable: TranspositionTable,
    p1Token: String,
    p2Token: String,
    minOrMax: String): (Boolean, Int) = {
    val generalizedBoard = boardState.map {x =>
      if(x == p1Token) {
        p1Token
      } else if(x == p2Token) {
        p2Token
      } else {
        "-"
      }
    }
    val path = if(minOrMax == "min") ttTable.min else ttTable.max
    val boardKey = generalizedBoard.foldLeft("")(_ + _)
    val keyPresent = path.contains(boardKey)
    if(keyPresent) {
      val score = path(boardKey)
      (true, score)
    } else {
      (false, 0)
    }
  }

  def getComputerMove(
    origBoardState: List[String],
    maxPlayerToken: String,
    minPlayerToken: String,
    currentPlayerToken: String,
    ttTable: TranspositionTable): Int = {

    val boardSize = origBoardState.length
    val alphaBeta = new AlphaBeta

    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {

      val depthLimit = if(boardSize > 9) 6 else 9

      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)

      val scores = openMoves.map(move =>
        //maxpath
        if(depth >= depthLimit) {
          Map(move -> 0)
        } else if(curT == maxT) {
          val maxScore = 1000 - depth
          val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val maxWin = Board.checkWin(maxBoardMove)
          val maxTie = Board.checkTie(maxBoardMove)
          if(maxWin) {
            val maxTranspositions = getBoardTranspositions(maxBoardMove, maxScore, maxT, minT)
            saveTranspositions(ttTable, maxTranspositions, "max")
            Map(move -> maxScore)
          } else if(maxTie) {
            val maxTieTranspositions = getBoardTranspositions(maxBoardMove, 0, maxT, minT)
            saveTranspositions(ttTable, maxTieTranspositions, "max")
            Map(move -> 0)
          } else {
            val checkTransTable = checkTransposition(maxBoardMove, ttTable, maxT, minT, "max")
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
            val minTranspositions = getBoardTranspositions(minBoardMove, minScore, maxT, minT)
            saveTranspositions(ttTable, minTranspositions, "min")
            Map(move -> minScore)
          } else if(minTie) {
            val minTieTranspostitions = getBoardTranspositions(minBoardMove, 0, maxT, minT)
            saveTranspositions(ttTable, minTieTranspostitions, "min")
            Map(move -> 0)
          } else {
            val checkTransTable = checkTransposition(minBoardMove, ttTable, maxT, minT, "min")
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
