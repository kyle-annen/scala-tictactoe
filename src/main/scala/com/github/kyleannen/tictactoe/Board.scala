package com.github.kyleannen.tictactoe

//Board handles the board initialization and returns states of the board
object Board {

  def initBoard(dimension: Int): List[String] = {
    List.range(1, dimension + 1, 1).map(x => x.toString)
  }

  def returnValidInputs(board: List[String]) = {
    val validValues = List.range(1, board.length + 1, 1).map(x => x.toString)
    val openMoves = board.filter(x => validValues.contains(x))
    openMoves
  }

  def returnDimension(board: List[String]): Int = {
    Math.sqrt(board.length).toInt
  }

  def returnRows(board: List[String]): List[List[String]] = {
    val rowNum: Int = returnDimension(board)
    board.grouped(rowNum).toList
  }

  def returnColumns(board: List[String]): List[List[String]] = {
    val colNum: Int = returnDimension(board)
    val boardLen: Int = board.length
    List.range(1, colNum + 1, 1).map(x => List.range(x, boardLen + 1, colNum).map(x => board(x - 1)))
  }

  def returnDiagonals(board: List[String]): List[List[String]] = {
    val diaLen: Int = returnDimension(board)
    val boardLen: Int = board.length
    val dia1: List[String] = List.range(1, boardLen + 1, diaLen + 1).map(x => board(x - 1).toString)
    val dia2: List[String] = List.range(diaLen, boardLen - 1, diaLen - 1).map(x => board(x - 1).toString)
    List(dia1, dia2)
  }

  def checkSets(sets: List[List[String]]): Boolean = {
    val setBooleans: List[Boolean] = sets.map(set => set.forall(_ == set.head))
    setBooleans.foldLeft(setBooleans(0))(_ || _)
  }

  def checkWin(board: List[String]): Boolean = {
    val diaBool: Boolean = checkSets(returnDiagonals(board))
    val rowBool: Boolean = checkSets(returnRows(board))
    val colBool: Boolean = checkSets(returnColumns(board))
    diaBool || rowBool || colBool
  }

  def checkSpace(board: List[String]): Boolean = returnValidInputs(board).nonEmpty

  def checkTie(board: List[String]): Boolean = {
    val win = checkWin(board)
    returnValidInputs(board).isEmpty && !win
  }

  def gameOver(board: List[String]): Boolean = {
    checkWin(board) || !checkSpace(board)
  }

  def getWinner(board: List[String]): String = {
    val allSets = returnDiagonals(board) ::: returnColumns(board) ::: returnRows(board)
    for(set <- allSets) {
      if(set.forall(_ == set.head)) {
        return set.head
      }
    }
    "none"
  }
}