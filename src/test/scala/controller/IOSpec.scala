package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class IOSpec extends FunSpec {

  def testInput1(): String = "test"
  def testInput2(): String = " test "

  describe("getUserInput") {
    it("should return the users input") {
      val expected = "test"
      val actual = IO.getUserInput(testInput1)
      assert(actual === expected)
    }

    it("trims the user input") {
      val expected = "test"
      val actual = IO.getUserInput(testInput2)
      assert(actual === expected)
    }
  }

  describe("getValidMove") {
    it("should not fail on invalid input") (pending)

    it("should proceed on valid input") (pending)
  }


}




