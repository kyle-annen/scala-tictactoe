package tictactoe 

import org.scalatest._

class ViewSpec extends FunSpec {

  val TestBlankBoard = List(
    1,2,3,
    4,5,6,
    7,8,9)
  def testPrint(s: String): String = s

  describe("renderWhitespace") {
    it("renders n number of linebreaks") {
      val expected = "\n\n"
      val actual = View.renderWhitespace(testPrint, 2)

      assert(actual === expected)  
    } 
  }

  describe("renderDialog") {
    it("renders dialog one string without line breaks") {
      val expected = "this is a test"
      val actual = View.renderDialog(testPrint, 0, "this is a test") 
      assert(actual === expected)
    }

    it("add leading space indicated") {
      val testString = "added a space"
      val expected = " added a space"
      val actual = View.renderDialog(testPrint, 1, testString)

      assert(actual === expected)
    }

    it("add line breaks for multiple string args passed") {
      val expected = "1\n2\n3"
      val actual = View.renderDialog(testPrint, 0, "1", "2", "3")

      assert(actual === expected)
    }

    
  }

  describe("formatBoard") (pending)


  
}
