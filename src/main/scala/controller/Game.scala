package tictactoe

object Game extends App {
    
    
    View.renderDialog(Dialog.en("greeting"))
    View.renderWhitespace(3)
    View.renderBoard(View.formatBoard(Board.initBoard(9),3),3)
    IO.getValidMove(
        Board.returnValidInputs(Board.initBoard(9)),
        Dialog.en("inputPrompt"),
        Dialog.en("invalidPlay")
    )
    
}