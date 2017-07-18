package tictactoe

object AI {

  class AlphaBeta {
    var alpha: Double = Double.NegativeInfinity
    var beta: Double = Double.PositiveInfinity
  }

  class TranspositionTable {
    var min = scala.collection.mutable.Map[String, Int]()
    var max = scala.collection.mutable.Map[String, Int]()
  }

  def getBoardTranspositions(
    board: List[String],
    score: Int,
    p1Token: String,
    p2Token: String): List[Map[String, Int]] = {

    val generalizedBoard = board.map { x =>
      if(x == p1Token) {
        p1Token
      } else if(x == p2Token) {
        p2Token
      } else {
        "-"
      }
    }
    val rowLists = Board.returnRows(generalizedBoard)
    val b1 = rowLists.flatten.foldLeft("")(_ + _)
    val b2 = rowLists.transpose.flatten.foldLeft("")(_ + _)
    val b3 = b1.reverse
    val b4 = b2.reverse

    val transpositions = List(
      Map(b1 -> score),
      Map(b2 -> score),
      Map(b3 -> score),
      Map(b4 -> score)
    )
    transpositions
  }


  def getComputerMove(
    origBoardState: List[String],
    maxPlayerToken: String,
    minPlayerToken: String,
    currentPlayerToken: String): Int = {

    val boardSize = origBoardState.length
    val alphaBeta = new AlphaBeta

    def miniMax(
      currentBoard: List[String],
      depth: Int,
      maxT: String,
      minT: String,
      curT: String): Map[Int, Int]  = {

      val depthLimit = 100

      val openMoves = Board.returnValidInputs(currentBoard).map(x => x.toInt - 1)

      val scores = openMoves.map(move =>
        //maxpath
        if(depth >= depthLimit) {
          Map(move -> 0)
        } else if(curT == maxT) {
          val maxScore = 1000 - depth
          val maxBoardMove: List[String] = currentBoard.map(x => if(x == (move+1).toString) curT else x)
          val maxWin = Board.checkWin(maxBoardMove)
          val maxTie = Board.checkTie(maxBoardMove)
          if(maxWin) {
            Map(move -> maxScore)
          } else if(maxTie) {
            Map(move -> 0)
          } else {
            val mmResult = miniMax(maxBoardMove, depth + 1, maxT, minT, minT)
            val mmScore = mmResult(mmResult.keys.head)
            Map(move -> mmScore)
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
            val mmResult = miniMax(minBoardMove, depth + 1, maxT, minT, maxT)
            val mmScore = mmResult(mmResult.keys.head)
            Map(move -> mmScore)
          }
        }
      )

      val mapScores = scores.flatten.toMap

      if(curT == maxT) {
        val maxTupleScore = mapScores.maxBy(_._2)
        val maxMapScore = Map(maxTupleScore._1 -> maxTupleScore._2)
        maxMapScore
      } else {
        val minTupleScore = mapScores.minBy(_._2)
        val minMapScore = Map(minTupleScore._1 -> minTupleScore._2)
        minMapScore
      }
    }
    //call the recursive function
    val result = miniMax(origBoardState, 1, maxPlayerToken, minPlayerToken, maxPlayerToken)
    result.keys.head
  }

}
// val board = List("X","O","X","X","O","X","7","8","9")
