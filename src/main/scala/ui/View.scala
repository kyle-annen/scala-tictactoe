package tictactoe

object View {

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

  def formatRow(row: List[String], cellWidth: Int): String = {
    val leftPad = (cellWidth - 1) / 2
    val bigNumRightPad = leftPad - 1
    val formatCells = row.map { x =>
      if(x.length < 2) {
        (" " * leftPad) + x + (" " * leftPad) + "|"
      } else {
        (" " * leftPad) + x + (" " * bigNumRightPad) + "|"
      }
    }
    val formatedRow = formatCells.foldLeft("")((a,b) => a + b).dropRight(1)
    formatedRow
  }

  def padLeft(s: String, n: Int): String = " " * n + s

  def renderBoard(
    output: String => Any,
    fBoard: List[List[String]],
    leftPadding: Int) = {
    val width: Int = fBoard.length
    val cellWidth: Int = if(width > 3) 5 else 3
    val hLine: String = ((("=" * cellWidth) + "+") * width).dropRight(1)
    val hLineLen: Int = hLine.length
    val nRows: Int = fBoard.length
    val formatedRows = fBoard.map(row => formatRow(row, cellWidth))
    val paddedRows = formatedRows.map(row => padLeft(row, leftPadding))
    val padHLine = padLeft(hLine, leftPadding)
    val formatedBoard = paddedRows.foldLeft("")(
      (a,b) => a + "\n" + b + "\n" + padHLine
    ).dropRight(hLineLen)
    output(formatedBoard)
  }
}




