package tictactoe

object Board {
    val initialBoardState: List[Any] = List(1,2,3,4,5,6,7,8,9)

    def returnDimension(board: List[Any]): Int = {
        Math.sqrt(board.length).toInt
    }
    //returns a list of the row lists
    def returnRows(board: List[Any]): List[List[Any]] = {
        val rowNum = returnDimension(board)
        board.grouped(rowNum).toList
    }
    def checkRows() {}
    
    //returns a list of column lists
    def returnColumns(board: List[Any]): List[List[Any]] = {
        val colNum = returnDimension(board)
        val boardLen = board.length
        List.range(1, colNum + 1, 1).map(x => List.range(x, boardLen + 1, colNum))
    }
    def checkColumns() {}

    def returnDiagonals() = {

    }
    def checkDiagnals() {}

}