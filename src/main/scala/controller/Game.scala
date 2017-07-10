package tictactoe
//game object contains the game loop and runs the game
object Game {
  def initPlayers(): Map[Int, String] = Map(1 -> "X", 2 -> "O")

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
    val langSelection =
      IO.getValidMove(
        langOptions,
        Dialog.lang("EN")("selectLang"),
        Dialog.lang("EN")("invalidPlay"),
        output,
        getInput,
        leftPadding,
        1)
    langSelection
  }

  def go(
    board: List[String],
    players: Map[Int, String],
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

    val userPlay: String =
      IO.getValidMove(
        validPlays,
        inputPrompt,
        invalidPlay,
        output,
        getInput,
        leftPadding,
        loopCount)

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
    players: Map[Int, String],
    currentPlayer: Int,
    output: String => Any,
    leftPadding: Int,
    whiteSpace: Int,
    getInput: Int => String,
    loopCount: Int): Boolean = {
      View.renderWhitespace(output, 100)
      val selectedLanguage = setLanguage(output, 15, getInput)

      go(
        board, players, Dialog.lang(selectedLanguage), false, currentPlayer,
        output, leftPadding, whiteSpace, getInput, loopCount)
    }

  def main(args: Array[String]): Unit = {
    setup(
      Board.initBoard(9), initPlayers(),
      1, println, 15, 100, IO.getInput, 1)
  }
}
