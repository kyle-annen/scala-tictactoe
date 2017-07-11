package tictactoe
//game object contains the game loop and runs the game
object Game {
  def initPlayers(): Map[Int, (String, String)] = Map(1 -> ("", ""), 2 -> ("",""))

  def setLanguage(
    output: String => Any,
    leftPadding: Int,
    getInput: Int => String ) = {
    View.renderDialog(output, leftPadding, Dialog.lang("EN")("greeting"))
    View.renderWhitespace(output, 2)
    val langOptions = Dialog.lang.keys.toList
    for(option <- langOptions) {
      View.renderDialog(output, leftPadding, " - " + option)
    }
    val langSelection = IO.getValidMove(
      langOptions,
      Dialog.lang("EN")("selectLang"),
      Dialog.lang("EN")("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    langSelection
  }

  def setPlayer(
    output: String => Any, 
    leftPadding: Int, 
    getInput: Int => String,
    dialogLang: Map[String, String],
    playerNum: Int,
    playerToken: String) = {

    val pAnnounce = dialogLang("playerAnnounce") + playerNum.toString
    val pPrompt = dialogLang("selectPlayerType")
    val pHuman = dialogLang("pTypeHuman")
    val pComp = dialogLang("pTypeComputer")
    val prompt = pAnnounce + "\n" + pPrompt + "\n" + pHuman + "\n" + pComp 
    val pTypeOptions: List[String] = List("1","2")
    val playerType = IO.getValidMove(
      pTypeOptions, 
      prompt,
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    val pType = if(playerType == "1") "human" else "computer"
    val player = Map(playerNum -> (pType, playerToken))
    player
  }

  def go(
    board: List[String],
    players: Map[Int, (String, String)],
    dialogLang: Map[String, String],
    gameOver: Boolean,
    currentPlayer: Int,
    output: String => Any,
    leftPadding: Int,
    whiteSpace: Int,
    getInput: Int => String,
    loopCount: Int): Boolean = {

    View.renderWhitespace(output, whiteSpace)

    val fBoard = View.formatBoard(board)
    View.renderBoard(output, fBoard, leftPadding)

    val playerNumAnnounce: String = dialogLang("playerAnnounce") + currentPlayer
    View.renderDialog(output, leftPadding, playerNumAnnounce)

    val validPlays: List[String] = Board.returnValidInputs(board)
    val inputPrompt: String = dialogLang("inputPrompt")
    val invalidPlay: String = dialogLang("invalidPlay")
    val playerType: String = players(currentPlayer)._1
    val userToken: String = players(currentPlayer)._2
    val oppToken: String = if(userToken == "X") "O" else "X"
    //get the move value
    val boardMove = if(playerType == "human") {
      //human move
      val humanPlay: String =
        IO.getValidMove(
          validPlays,
          inputPrompt,
          invalidPlay,
          output,
          getInput,
          leftPadding,
          loopCount)
      humanPlay
    } else {
      //AI computer move
      val compPlay: Int = AI.getComputerMove(board, userToken, oppToken, userToken) + 1
      compPlay
    }
    
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
      true
    } else {
      val nextPlayer: Int = if(currentPlayer == 1) 2 else 1
      val newLoopCount = loopCount + 1

      go(
        updatedBoard, players, dialogLang, false, nextPlayer, output,
        leftPadding, whiteSpace, getInput, newLoopCount)
    }
  }

  def setup(
    board: List[String],
    currentPlayer: Int,
    output: String => Any,
    leftPadding: Int,
    whiteSpace: Int,
    getInput: Int => String,
    loopCount: Int): Boolean = {
      View.renderWhitespace(output, whiteSpace)
      val selectedLanguage = setLanguage(output, leftPadding, getInput)
      val gameLang = Dialog.lang(selectedLanguage)

      val player1 = setPlayer(println, leftPadding, getInput, gameLang, 1, "X")
      val player2 = setPlayer(println, leftPadding, getInput, gameLang, 2, "O")
      val players = List(player1, player2).flatten.toMap

      go(
        board, players, gameLang, false, currentPlayer,
        output, leftPadding, whiteSpace, getInput, loopCount)
    }

  def main(args: Array[String]): Unit = {
    setup(Board.initBoard(9), 1, println, 15, 100, IO.getInput, 1)
  }
}
