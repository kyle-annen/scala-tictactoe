package tictactoe

object IO {

    def getUserInput(): String = {
        val userInput = scala.io.StdIn.readLine().trim
        userInput
    }

    def getValidMove(validValues: List[Any], inputPrompt: String, invalidPlay: String) {

        def go(validity: Boolean, input: String ): String  = {
            if(validity) {
                input 
            } else {
                View.renderDialog(invalidPlay)
                View.renderDialog(inputPrompt)
                val input: String = getUserInput()
                val inputValidity = validValues.contains(input.toInt)
                go(inputValidity, input)
            }
        }

        go(false, "placeholder")
    }
}