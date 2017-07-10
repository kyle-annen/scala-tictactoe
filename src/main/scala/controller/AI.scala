package tictactoe

object AI {

  def getComputerMove(
    origBoardState: List[String], 
    maxPlayerToken: String, 
    minPlayerToken: String,
    currentPlayerToken: String): Int = {

    var alpha: Int = 0 
    var beta: Int = 0
    
    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {
      
      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)

      val scores = openMoves.map( move =>
        //maxpath
        if(curT == maxT) {
          val maxScore = 1000 - depth
          //alpha prune
          if(maxScore <= alpha) {
            Map(move -> 0)
          } else {
            val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
            val maxWin = Board.checkWin(maxBoardMove)
            val maxTie = Board.checkTie(maxBoardMove)
            if(maxWin) {
              if(maxScore > alpha) alpha = maxScore
              Map(move -> maxScore)
            } else if(maxTie) {
              Map(move -> 0)
            } else {
              miniMax(maxBoardMove, depth + 1, maxT, minT, minT)
            }
          }
        //min path
        } else {
          val minScore = -1000 + depth
          //beta prune
          if(minScore >= beta) {
            Map(move -> 0)
          } else {
            val minBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
            val minWin = Board.checkWin(minBoardMove)
            val minTie = Board.checkTie(minBoardMove)
            if(minWin) {
              if(minScore < beta) beta = minScore
              Map(minScore -> minScore)
            } else if(minTie) {
              Map(move -> 0)
            } else {
              miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
            }
          }
        }
      )
      // return the max map here  
      val mapScores = scores.flatten.toMap  
      val tupleScore = mapScores.maxBy(_._2)
      val mapScore = Map(tupleScore._1 -> tupleScore._2)
      mapScore

    }
    //call the recursive function
    val result = miniMax(origBoardState, 1, maxPlayerToken, minPlayerToken, maxPlayerToken)
    result.keys.head
  }
}
// val board = List("X","O","X","X","O","X","7","8","9") 
