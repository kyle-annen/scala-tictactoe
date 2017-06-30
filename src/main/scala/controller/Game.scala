package tictactoe
//game object congains the game loop and runs the game
object Game extends App {
    //initializes the player map
    def initPlayers() = Map(1 -> "X", 2 -> "O")
    //game recursive logic loop
    def go(board: List[Any], players: Map[Int, String], 
           dialogLang: Map[String, String], gameOver: Boolean, currentPlayer: Int) {
        //clear console
        View.renderWhitespace(50)
        //format the board for rendering
        val fBoard = View.formatBoard(board, 3)
        //render the board
        View.renderBoard(fBoard, 3)
        //construct the player and number
        val playerNumAnnounce: String = dialogLang("playerAnnounce") + currentPlayer
        //render the player announcement and turn prompt
        View.renderDialog(playerNumAnnounce)
        //get valid plays 
        val validPlays: List[Any] = Board.returnValidInputs(board)
        //set values for concise method calls
        val inputPrompt: String = dialogLang("inputPrompt")
        //set values for concise method calls
        val invalidPlay: String = dialogLang("invalidPlay")
        //get user play
        val userPlay: String = IO.getValidMove(validPlays, inputPrompt,  invalidPlay)
        val boardMove: Int = userPlay.toInt
        //get the user token
        val userToken: String = players(currentPlayer)
        //create new board with updated value
        val updatedBoard: List[Any] = board.patch(boardMove - 1, userToken, 1)
        //get boolen for whether the game is over
        val gameOver: Boolean = Board.gameOver(updatedBoard)
        //if the game is over, use the end progression
        if(gameOver) {
            //render whitespace
            View.renderWhitespace(50)
            //set the formatting for the end board
            val endBoard: List[List[Any]] = View.formatBoard(updatedBoard, 3)
            //render the last board to the console
            View.renderBoard(endBoard, 3)
            //render the game over message
            View.renderDialog("The game is over!")
            //get the boolean for whether the game is won or not
            val isWin: Boolean = Board.checkWin(updatedBoard)
            //conditional loop to render a winner or a tie
            if (isWin) {
                //render the winning dialog
                View.renderDialog(playerNumAnnounce, dialogLang("win"))
            } else {
                //render the tie dialog
                View.renderDialog(dialogLang("tie"))
            }
            //render the ending whitespace
            View.renderWhitespace(15)
        //if the game is not over, progress with recursion
        } else {
            //set the next player
            val nextPlayer: Int = if(currentPlayer == 1) 2 else 1
            //recursively call the game the game loop
            go(updatedBoard, players, dialogLang, false, nextPlayer)
        }
    }
    //greet the player
    View.renderDialog(Dialog.en("greeting"))
    //run the recursive game loop
    go(Board.initBoard(9), initPlayers(), Dialog.en, false, 1)
}