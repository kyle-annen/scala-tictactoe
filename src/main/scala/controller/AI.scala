package tictactoe

object AI {

  def getComputerMove(
    origBoardState: List[String],
    maxPlayerToken: String,
    minPlayerToken: String,
    currentPlayerToken: String): Int = {

    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {

      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)
      println("OpenMoves: " + openMoves)

      val scores = openMoves.map(move =>
        //maxpath
        if(curT == maxT) {
          val maxScore = 1000 - depth
          val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val maxWin = Board.checkWin(maxBoardMove)
          val maxTie = Board.checkTie(maxBoardMove)
          if(maxWin) {
            Map(move -> maxScore)
          } else if(maxTie) {
            Map(move -> 0)
          } else {
            miniMax(maxBoardMove, depth + 1, maxT, minT, minT)
          }
        //min path
        } else {
          val minScore = -1000 + depth
          val minBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val minWin = Board.checkWin(minBoardMove)
          val minTie = Board.checkTie(minBoardMove)
          if(minWin) {
            Map(move -> minScore)
          } else if(minTie) {
            Map(move -> 0)
          } else {
            miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
          }
        }
      )
      // return the max map here
      val mapScores = scores.flatten.toMap
      if(curT == maxT) {
        val maxTupleScore = mapScores.maxBy(_._2)
        val maxMapScore = Map(maxTupleScore._1 -> maxTupleScore._2)
        println("Optimizing Player: " + maxT)
        println("Turn: " + curT)
        View.renderBoard(println, View.formatBoard(currentBoard), 15)
        println("------scores------")
        println("Max Map Score" + maxMapScore)
        maxMapScore
      } else {
        val minTupleScore  = mapScores.minBy(_._2)
        val minMapScore = Map(minTupleScore._1 -> minTupleScore._2)
        println("Optimizing Player: " + maxT)
        println("Turn: " + curT)
        View.renderBoard(println, View.formatBoard(currentBoard), 15)
        println("------scores------")
        println("Max Map Score" + minMapScore)
       minMapScore
      }
    }
    //call the recursive function
    val result = miniMax(origBoardState, 1, maxPlayerToken, minPlayerToken, maxPlayerToken)
    println("-----------------Computer Choice----------------")
    println(result.keys.head)
    result.keys.head

  }
}
// val board = List("X","O","X","X","O","X","7","8","9")
