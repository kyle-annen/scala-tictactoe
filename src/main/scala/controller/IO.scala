package tictactoe
//IO handles the user input
object IO {

  def getInput(): String = {
    scala.io.StdIn.readLine().trim
  }

  def getUserInput(getInput: () => String): String = {
    val userInput = getInput().trim 
      userInput
    }
  //prompts the user input, validates against values, reprompts if incorrect input,
  //and returns the value
    def getValidMove(
      validValues: List[String], 
      inputPrompt: String, 
      invalidPlay: String,
      output: String => Any,
      getInput: Unit => String,
      leftPadding: Int) =  {
      
      def go(validity: Boolean, input: String ): String  = {
        View.renderDialog(output, leftPadding, inputPrompt)
        if(validity) {
          input
        } else {
          View.renderDialog(output,leftPadding,inputPrompt)
          val input: String = getInput() 
          try {
            val inputValidity = validValues.contains(input.toInt)
            go(inputValidity, input)
          } catch {
            case _: Throwable => {
            View.renderDialog(output,leftPadding,invalidPlay)
            go(false, input)
          }
        }
      }
    }

  go(false, "none")
  }
}