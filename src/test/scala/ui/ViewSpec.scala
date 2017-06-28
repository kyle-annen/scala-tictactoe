package tictactoe 

import org.scalatest._

class ViewSpec extends FunSpec {

    describe("formatRow") {
      it("should correctly format row") {
        val testRowList: List[Any] = List(1,2,3)
        val exected: String = " 1 | 2 | 3 "
        val actual: String = View.formatRow(testRowList)

        assert(actual === expected)
      }
    }

    describe("renderWhitespace") {}

    describe("formatBoard") {
      it("Should take a List and return a list of lists in grouping indicated") {
        val testBoard: List[Int] = List(1,2,3,4,5,6,7,8,9)
        val expected: List[Any] = List(List(1,2,3), List(4,5,6), List(7,8,9))
        val actual = View.formatBoard(testBoard)
        
        assert( actual === expected)
      }
    }


    describe("renderBoard") {
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
          
        }
      }
    }
    

}
