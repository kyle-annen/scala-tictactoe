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
    it("renders dialog strings to console") {
    

    }
  }


  
}
