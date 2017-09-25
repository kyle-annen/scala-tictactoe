package com.github.kyleannen.tictactoe

object TTTable {
  class TranspositionTable {
    var min = scala.collection.mutable.Map[String, Int]()
    var max = scala.collection.mutable.Map[String, Int]()
  }

  def swapTranspositionKeys(key: String, p1Token: String, p2Token: String): String = {
    val swapArray = key.split("").map { char =>
      char match {
        case `p1Token` => p2Token
        case `p2Token` => p1Token
        case _ => char
      }
    }
    swapArray.foldLeft("")(_ + _)
  }


  def getBoardTranspositions(
    board: List[String],
    score: Int,
    p1Token: String,
    p2Token: String): Map[String, List[(String, Int)]] = {

    val generalizedBoard = board.map { x =>
      x match {
        case `p1Token` => p1Token
        case `p2Token` => p2Token
        case _ => "-"
      }
    }

    val rowLists = Board.returnRows(generalizedBoard)
    val board1 = rowLists.flatten.foldLeft("")(_ + _)
    val board2 = rowLists.transpose.flatten.foldLeft("")(_ + _)
    val board3 = board1.reverse
    val board4 = board2.reverse
    val oppBoard1 = swapTranspositionKeys(board1, "X", "O")
    val oppBoard2 = swapTranspositionKeys(board2, "X", "O")
    val oppBoard3 = swapTranspositionKeys(board3, "X", "O")
    val oppBoard4 = swapTranspositionKeys(board4, "X", "O")

    val transpositions = List(
      board1 -> score,
      board2 -> score,
      board3 -> score,
      board4 -> score)
    val oppositeTranspositions = List(
      oppBoard1 -> -score,
      oppBoard2 -> -score,
      oppBoard3 -> -score,
      oppBoard4 -> -score)
    Map("current" -> transpositions, "opposite" -> oppositeTranspositions)
  }

  def saveTranspositions(
    tt: TranspositionTable,
    values: Map[String,List[(String, Int)]],
    minOrMax: String): Unit = {
    val currentPath = if(minOrMax == "min") tt.min else tt.max
    val oppPath = if(minOrMax == "min") tt.max else tt.min
    for (value <- values("current")) {
      currentPath += value
    }
    for(value <- values("opposite")) {
      oppPath += value
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
}


