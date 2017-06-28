package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class BoardSpec extends FunSpec {

    describe("initialBoardState") {
        it("should be a list of length 9") {
            Board.initialBoardState.length should be (9)
        }
    }

    describe("returnRows") {
        it("should return a list of row lists") {
            val testBoard: List[Any] = List(1,2,3,4,5,6,7,8,9)
            val expected: List[List[Any]] = List(List(1,2,3), List(4,5,6), List(7,8,9))
            val actual: List[List[Any]] = Board.returnRows(testBoard)

            assert( actual === expected )
        }
    }

    describe("returnColumns") {
        it("should return a list of column lists") {
            val testBoard: List[Any] = List(1,2,3,4,5,6,7,8,9)
            val expected: List[List[Any]] = List(List(1,4,7), List(2,5,8), List(3,6,9))
            val actual: List[List[Any]] = Board.returnColumns(testBoard)

            assert( actual === expected )

        }
    }
}