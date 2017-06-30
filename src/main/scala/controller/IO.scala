package tictactoe
//IO handles the user input
object IO {
    //returns the users input from the console
    def getUserInput(): String = {
        //get the value from the user
        val userInput = scala.io.StdIn.readLine().trim
        //implicit return
        userInput
    }
    //prompts the user input, validates against values, reprompts if incorrect input,
    //and returns the value
    def getValidMove(validValues: List[Any], inputPrompt: String, invalidPlay: String) =  {
        //recursive lopt
        def go(validity: Boolean, input: String ): String  = {
            //if the input is valid, return the value
            if(validity) {
                input
            //if not valid
            } else {
                //re-prompt the user
                View.renderDialog(inputPrompt)
                //get the input from the user
                val input: String = getUserInput()
                //try catch handles invalid inputs
                try {
                    //checks the validity of the input
                    //***this will FAIL if not a number
                    val inputValidity = validValues.contains(input.toInt)
                    //recursively calls the loop
                    go(inputValidity, input)
               } catch {
                    //for not a number value, or other failure
                    case _: Throwable => {
                        //inform the user of the ivalid play
                        View.renderDialog(invalidPlay)
                        //recursively call the loop
                        go(false, input)
                    }
               }
            }
        }

        go(false, "none")
    }
}