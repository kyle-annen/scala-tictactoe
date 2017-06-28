package tictactoe 

object View extends App {

  val board = List(1,2,3,4,5,6,7,8)

  def formatRow(row: List[Any]): String = {
    val struct: String = " %d | %d | %d "
    struct.format(row(0), row(1), row(2))
  }

  def renderWhitespace(n: Int): Unit = {
    for( i <- 1 to 10 ) {
      println()
    }
  }

  def renderBoard(board: List[Any]): Unit = {
    val row1 = board.slice(0,3)
    val row2 = board.slice(2,6)
    val row3 = board.slice(5,9)
    val hLine = "===+===+==="

    renderWhitespace(10)
    println(formatRow(row1))
    println(hLine)
    println(formatRow(row2))
    println(hLine)
    println(formatRow(row3))
    renderWhitespace(10)
  }

  
  renderBoard(board)
}




