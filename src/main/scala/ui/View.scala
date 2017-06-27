package view 

object View extends App {

  val greeting: String = "Welcome to TicTacToe"

  val moves = List.fill(9)(null)

  def render(x: String) {
    println(x)
  }

  def createBoard(moves: List ) {
    moves.flatMap(move => if())


  }

  def renderBoard(board: List) {


  }

  render(greeting)
}




