package tictactoe 
//view handles all the rendering to the console


object View {

  def print(s: String): Unit = Console.println(s)
  
  //renders the number of blank lines provided as an argument
  def renderWhitespace(output: String => Any, n: Int) = {
    val wSpace: String = "\n" * n
    output(wSpace)
  }

  //renders strings of text to the console
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

  //formats the board into a list of lists, one for each row
  def formatBoard(board: List[String]): List[List[String]] = {
    val grouping = Math.sqrt(board.length).toInt
    board.grouped(grouping).toList
  }
  //formats rows into strings that represent a tictactoe row 
  def formatRow(row: List[String]): String = {
    val noLeadingSpace: String = row mkString " | "
    val fBoard: String = " " + noLeadingSpace
    fBoard
  }

  def padLeft(s: String, n: Int): String = " " * n + s

  //renders the board to the console
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




