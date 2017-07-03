package tictactoe
//IO handles the user input
object IO {

  def getInput(callCount: Int = 0): String = {
    scala.io.StdIn.readLine().trim
  }

  def getUserInput(getInput: Int => String): String = {
    val userInput = getInput(0).trim 
      userInput
    }
  //prompts the user input, validates against values, reprompts if incorrect input,
  //and returns the value
    def getValidMove(
      validValues: List[String], 
      inputPrompt: String, 
      invalidPlay: String,
      output: String => Any,
      getInput: Int => String,
      leftPadding: Int) =  {
      
      def go(validity: Boolean, input: String, callCount: Int = 0): String  = {

        if(validity) {
          input
        } else {
          View.renderDialog(output,leftPadding,inputPrompt)
          val input: String = getInput(callCount) 
          try {
            val inputValidity = validValues.contains(input)
            val newCallCount = callCount + 1
            go(inputValidity, input, newCallCount)
          } catch {
            case _: Throwable => {
            View.renderDialog(output,leftPadding,invalidPlay)
            val nextCallCount = callCount + 1
            go(false, input, nextCallCount)
          }
        }
      }
    }

  go(false, "none", 0)
  }
}