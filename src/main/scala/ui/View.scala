package tictactoe 
//view handles all the rendering to the console
object View {
  //renders the number of blank lines provided as an argument
  def renderWhitespace(n: Int): Unit = {
    for( i <- 1 to n ) {
      println()
    }
  }

  //renders strings of text to the console
  def renderDialog(dialog: String*): Unit = {
    for (string <- dialog) {
      val paddedString: String = "   " + string
      println(paddedString)
    }
  }

  //formats the board into a list of lists, one for each row
  def formatBoard(board: List[Any], grouping: Int): List[List[Any]] = {
    board.grouped(grouping).toList
  }
  //formats rows into strings that represent a tictactoe row 
  def formatRow(row: List[Any]): String = {
    val noLeadingSpace: String = row mkString " | "
    val fBoard: String = " " + noLeadingSpace
    fBoard
  }

  //renders the board to the console
  def renderBoard(fBoard: List[List[Any]], boardPadding: Int): Unit = {
    val hLine: String = "       ===+===+==="
    val nRows: Int = fBoard.length
    //renders white space above the board 
    renderWhitespace(boardPadding)
    //loop to render the board to the console
    for (i <- 0 to nRows - 1) {
      val row: List[Any] = fBoard(i)
      val formatedRow: String = "       " + formatRow(row)
      println(formatedRow)
      if (i < nRows - 1) 
        println(hLine)
    }
    //renders whitespace below the board
    renderWhitespace(boardPadding)
  }
}




