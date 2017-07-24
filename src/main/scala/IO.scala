package tictactoe

import scala.annotation.tailrec

object IO {

  //callCount agrument used purely for mocking / testing
  def getInput(callCount: Int = 0): String = {
    scala.io.StdIn.readLine().trim
  }

  def getUserInput(getInput: Int => String): String = {
    val userInput = getInput(0).trim
    userInput
  }

  def getValidMove(
    validValues: List[String],
    inputPrompt: String,
    invalidPlay: String,
    output: String => Any,
    getInput: Int => String,
    leftPadding: Int,
    callCount: Int): String = {

    @tailrec def go(input: String, callCount: Int): String  = {
      View.renderDialog(output, leftPadding, inputPrompt)
      val input: String = getInput(callCount)
      val inputValidity = validValues.contains(input)
      if (inputValidity) {
        input
      } else {
        View.renderDialog(output, leftPadding, invalidPlay)
        go(input, callCount + 1)
      }
    }
    go("none", callCount)
  }
}
