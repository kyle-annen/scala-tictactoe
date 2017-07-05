package tictactoe
//game object contains the game loop and runs the game
object Game extends App {
  def initPlayers(): Map[Int, String] = Map(1 -> "X", 2 -> "O")

  def setLanguage(
    output: String => Unit,
    leftPadding: Int) = {
    View.renderDialog(output, leftPadding, Dialog.lang("EN")("greeting"))
    View.renderWhitespace(output, 2)
    val langOptions = Dialog.lang.keys.toList
    for(option <- langOptions) {
      View.renderDialog(output, leftPadding, " - " + option)
    }
    val langSelection = 
      IO.getValidMove(
        langOptions,
        Dialog.lang("EN")("selectLang"),
        Dialog.lang("EN")("invalidPlay"),
        output,
        IO.getInput,
        leftPadding)
    langSelection
  }
  
  def go(
    board: List[String], 
    players: Map[Int, String], 
    dialogLang: Map[String, String], 
    gameOver: Boolean, 
    currentPlayer: Int,
    output: String => Unit,
    leftPadding: Int,
    whiteSpace: Int): Unit = {
    

    View.renderWhitespace(output, whiteSpace)

    val fBoard = View.formatBoard(board)
    View.renderBoard(output, fBoard, leftPadding)

    val playerNumAnnounce: String = dialogLang("playerAnnounce") + currentPlayer
    View.renderDialog(output, leftPadding, playerNumAnnounce)

    val validPlays: List[String] = Board.returnValidInputs(board)
    val inputPrompt: String = dialogLang("inputPrompt")
    val invalidPlay: String = dialogLang("invalidPlay")

    val userPlay: String = 
      IO.getValidMove(
        validPlays, 
        inputPrompt,  
        invalidPlay,
        output,
        IO.getInput,
        leftPadding)
        
    val boardMove: Int = userPlay.toInt
    val userToken: String = players(currentPlayer)
    val updatedBoard: List[String] = board.map(
      x => if (x == boardMove.toString) userToken else x
    )

    val gameOver: Boolean = Board.gameOver(updatedBoard)
    
    if(gameOver) {
      View.renderWhitespace(output, whiteSpace)
      val endBoard: List[List[String]] = View.formatBoard(updatedBoard)
      View.renderBoard(output, endBoard, leftPadding)
      View.renderDialog(output, leftPadding, dialogLang("gameOver"))
      val isWin: Boolean = Board.checkWin(updatedBoard)
      
      if (isWin) {
        View.renderDialog(output, leftPadding, playerNumAnnounce, dialogLang("win"))
      } else {
        View.renderDialog(output, leftPadding, dialogLang("tie"))
      }

      View.renderWhitespace(output, 5)
    } else {
      val nextPlayer: Int = if(currentPlayer == 1) 2 else 1
      go(
        updatedBoard, 
        players, 
        dialogLang, 
        false, 
        nextPlayer, 
        output,
        leftPadding, 
        whiteSpace)
    }
  }

  val selectedLanguage = setLanguage(println, 15) 

  go(
    Board.initBoard(9), 
    initPlayers(), 
    Dialog.lang(selectedLanguage), 
    false, 
    1,
    println,
    15,
    30)
}