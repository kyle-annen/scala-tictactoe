package tictactoe
//Board handles the board initialization and returns states of the board
object Board {
    
    //intitialilzes the board
    def initBoard(dimension: Int): List[Any] = {
        List.range(1, dimension + 1, 1)
    }

    //returns a list of valid inputs for the board
    def returnValidInputs(board: List[Any]) = {
        board.filter((x: Any) => x.getClass.getSimpleName == "Integer") 
    }

    //returns the dimensions of the board
    def returnDimension(board: List[Any]): Int = {
        Math.sqrt(board.length).toInt
    }
    //returns a list of the row lists
    def returnRows(board: List[Any]): List[List[Any]] = {
        val rowNum: Int = returnDimension(board)
        board.grouped(rowNum).toList
    }
    
    //returns a list of column lists
    def returnColumns(board: List[Any]): List[List[Any]] = {
        val colNum: Int = returnDimension(board)
        val boardLen: Int = board.length
        List.range(1, colNum + 1, 1).map(x => List.range(x, boardLen + 1, colNum).map(x => board(x - 1)))
    }

    //returns a list of diagonal lists
    def returnDiagonals(board: List[Any]): List[List[Any]] = {
        val diaLen: Int = returnDimension(board)
        val boardLen: Int = board.length
        val dia1: List[Any] = List.range(1, boardLen + 1, diaLen + 1).map(x => board(x - 1))
        val dia2: List[Any] = List.range(diaLen, boardLen - 1, diaLen - 1).map(x => board(x - 1))
        List(dia1, dia2)
    }

    //checks to see if a provided list of lists has a list with identical internal values
    def checkSets(sets: List[List[Any]]): Boolean = {
        val setBooleans: List[Boolean] = sets.map(set => set.forall(_ == set.head))
        setBooleans.foldLeft(setBooleans(0))(_ || _)
    }

    //returns a boolean, checks diagonals, columns and rows for identical values
    def checkWin(board: List[Any]): Boolean = {
        val diaBool: Boolean = checkSets(returnDiagonals(board))
        val rowBool: Boolean = checkSets(returnRows(board))
        val colBool: Boolean = checkSets(returnColumns(board))
        diaBool || rowBool || colBool
    }

    //returns true if open moves, false if board if full
    def checkSpace(board: List[Any]): Boolean = {
        board.map(x => x.getClass.getSimpleName == "Integer").foldLeft(false)(_ || _)
    }

    //return true if there is a tie, false if there is not tie
    def checkTie(board: List[Any]): Boolean = {
        checkSpace(board) && checkWin(board)
    }

    //return true if the game is over, false if not
    def gameOver(board: List[Any]): Boolean = {
        checkWin(board) || !checkSpace(board)
    }

    //returns a list, first value a boolean, second the text of the winner
    //if there is no winner, the text is "none"
    //ex. X wins, return List(true, "X")
    //ex. if no winner, reutnr List(false, "none")
    def getWinner(board: List[Any]): List[Any] = {
        val allSets = returnDiagonals(board) ::: returnColumns(board) ::: returnRows(board)
        for(set <- allSets) {
            if(set.forall(_ == set.head)) {
                return List(true, set.head)
            }
        }
        return List(false, "none")
    }
}