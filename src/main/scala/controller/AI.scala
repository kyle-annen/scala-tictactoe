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
      curT: String): Int = {
      
      val openMoves = Board.returnValidInputs(origBoardState).map(x => x.toInt)

      val scores = openMoves.map( move =>
        if(curT == maxT) {
          val maxScore = 1000 - depth
          //alpha prune
          if(maxScore <= alpha) {
            0
          } else {
            val maxBoardMove: List[String] = currentBoard.map(x => if(x == move) curT else x)
            val maxWin = Board.checkWin(maxBoardMove)
            val maxTie = Board.checkTie(maxBoardMove)
            if(maxWin) {
              maxScore
            } else if(maxTie) {
              0
            } else {
              miniMax(maxBoardMove, depth + 1, maxT, minT, minT)
            }
          }
        //min path
        } else {
          val minScore = -1000 + depth
          //beta prune
          if(minScore >= beta) {
            0
          } else {
            val minBoardMove: List[String] = currentBoard.map(x => if(x == move) curT else x)
            val minWin = Board.checkWin(minBoardMove)
            val minTie = Board.checkTie(minBoardMove)
            if(minWin) {
              minScore
            } else if(minTie) {
              0
            } else {
              miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
            }
          }
        }
      ) 
      scores.indexOf(scores.max)
    }
    //call the recursive function
    miniMax(origBoardState, 1, maxPlayerToken, minPlayerToken, maxPlayerToken)
  }
} 
