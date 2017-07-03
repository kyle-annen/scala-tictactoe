package tictactoe
//IO handles the user input
object IO {
//returns the users input from the console
/**
  def getUserInput(): String = {
  //get the value from the user
    val userInput = scala.io.StdIn.readLine().trim
    //implicit return
      userInput
    }
  //prompts the user input, validates against values, reprompts if incorrect input,
  //and returns the value
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
  */
}