package org.clojars.kyleannen.tictactoe

import org.scalatest.FunSpec

class IOSpec extends FunSpec {

  def testPrint(s: String): Unit = return

  def mockInput(callCount: Int): String = {
    callCount match {
      case 0 => "-1"
      case 1 => "23"
      case 2 => "0"
      case 3 => "X"
      case 4 => "1"
    }
  }

  describe("getUserInput") {
    it("can get the user's input") {
      def mockInputText(n: Int): String = "test"
      val expected: String = "test"
      val actual = IO.getUserInput(mockInputText)
      assert(actual === expected)
    }

    it("can get user input and eliminate spaces") {
      def mockInputText(n: Int): String = " test "
      val expected: String = "test"
      val actual = IO.getUserInput(mockInputText)
      assert(actual === expected)
    }
  }

  describe("getValidMove") {
    it("will not accept bad input") {
      val testString: String = "test string"
      val validList: List[String] = List("1","2","3")
      val expected: String = "1"
      val actual: String = IO.getValidMove(
        validList, testString, testString,
        testPrint, mockInput, 0, 0)
      assert(actual === expected)
    }
  }
}
