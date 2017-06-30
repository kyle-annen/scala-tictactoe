package tictactoe

object IO {

    def getUserInput(): String = {
        val userInput = scala.io.StdIn.readLine().trim
        userInput
    }

    def getValidMove(validValues: List[Any], inputPrompt: String, invalidPlay: String) =  {

        def go(validity: Boolean, input: String ): String  = {
            if(validity) {
                input
            } else {
                View.renderDialog(inputPrompt)
                val input: String = getUserInput()

                try {
                    val inputValidity = validValues.contains(input.toInt)
                    go(inputValidity, input)
               } catch {
                    case _: Throwable => {
                        View.renderDialog(invalidPlay)
                        go(false, input)
                    }
               }
            }
        }

        go(false, "none")
    }
}