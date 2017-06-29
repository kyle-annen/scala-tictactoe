package tictactoe

object Game extends App {
    
    
    View.renderDialog(Dialog.en("greeting"))
    View.renderWhitespace(3)
    View.renderDialog("Enter a word.")
    val userInput = scala.io.StdIn.readLine() 

    View.renderDialog("This was your word")
    View.renderDialog(userInput)
    
}