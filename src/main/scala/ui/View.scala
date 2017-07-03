package tictactoe 

object View {

  def print(s: String): Unit = Console.println(s)
  
  def renderWhitespace(output: String => Any, n: Int) = {
    val wSpace: String = "\n" * n
    output(wSpace)
  }

  def renderDialog(
    output: String => Any, 
    leftPadding: Int,
    dialog: String*) = {
    val sPad = " " * leftPadding
    val fDialog = dialog.foldLeft("")(
      (a, b) => a + sPad + b + "\n")
      .dropRight(1)
    
    output(fDialog)
  }

  def formatBoard(board: List[String]): List[List[String]] = {
    val grouping = Math.sqrt(board.length).toInt
    board.grouped(grouping).toList
  }

  def formatRow(row: List[String]): String = {
    val noLeadingSpace: String = row mkString " | "
    val fBoard: String = " " + noLeadingSpace
    fBoard
  }

  def padLeft(s: String, n: Int): String = " " * n + s

  def renderBoard(
    output: String => Any,
    fBoard: List[List[String]], 
    leftPadding: Int) = {
    val hLine: String = "===+===+==="
    val hLineLen: Int = hLine.length
    val nRows: Int = fBoard.length
    val formatedRows = fBoard.map(row => formatRow(row))
    val paddedRows = formatedRows.map(row => padLeft(row, leftPadding))
    val padHLine = padLeft(hLine, leftPadding)
    val formatedBoard = paddedRows.foldLeft("")(
      (a,b) => a + "\n" + b + "\n" + padHLine
    ).dropRight(hLineLen) 
    output(formatedBoard)
  }
}




