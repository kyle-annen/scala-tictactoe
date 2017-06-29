package tictactoe

object IO {

    def getUserInput(): String = {
        val userInput = scala.io.StdIn.readLine().trim
    }
}