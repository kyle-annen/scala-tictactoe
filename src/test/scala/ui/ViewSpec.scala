package tictactoe 

import org.scalatest._

class ViewSpec extends FunSpec {

  describe("renderWhitespace") {}

  describe("formatBoard") {
    it("should group a list into a list of list of length provided") {
      val grouping: Int = 3
      val testBoard: List[Any] = List(1,2,3,4,5,6,7,8,9)
      val expected: List[List[Any]] = List(List(1,2,3), List(4,5,6), List(7,8,9))
      val actual = View.formatBoard(testBoard, 3)

      assert( actual === expected)
    }
  }
  
  describe("renderBoard") {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
        
      
    }
  }
}
