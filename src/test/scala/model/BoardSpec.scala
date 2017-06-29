package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class BoardSpec extends FunSpec {

    describe("*initBoard") {
        it("it should return a List of the size indicated, from 1 to Dimension") {
            val expected: List[Any] = (1 to 9).toList 
            val actual = Board.initBoard(9)
            
            assert(actual === expected)
        }
    }

    describe("*returnRows") {
        it("should return a list of row lists") {
            val testBoard: List[Any] = (1 to 9).toList 
            val expected: List[List[Any]] = List(List(1,2,3), List(4,5,6), List(7,8,9))
            val actual: List[List[Any]] = Board.returnRows(testBoard)

            assert( actual === expected )
        }
    }

    describe("*returnColumns") {
        it("should return a list of column lists") {
            val testBoard: List[Any] = (1 to 9).toList 
            val expected: List[List[Any]] = List(List(1,4,7), List(2,5,8), List(3,6,9))
            val actual: List[List[Any]] = Board.returnColumns(testBoard)

            assert( actual === expected )

        }
    }

    describe("*returnDiagonals") {
        it("should return a list of diagonal lists") {
            val testBoard: List[Any] = List(1,2,3,4,5,6,7,8,9)
            val expected: List[List[Any]] = List(List(1,5,9), List(3,5,7))
            val actual: List[List[Any]] = Board.returnDiagonals(testBoard)

            assert( actual === expected )

        }
    }

    describe("*checkSets") {
        it("should return true if one list in a list of lists has identical values internally") {
            val testBoard: List[Any] = List("x","x","x",4,5,6,7,8,9)
            val expected: Boolean = true
            val actual = Board.checkSets(Board.returnRows(testBoard))

            assert( actual === expected)
        }
        
        it("should return return false if no list in a list of lists has identical values") {
            val testBoard: List[Any] = List("x","x","x",4,5,6,7,8,9)
            val expected: Boolean = false 
            val actual = Board.checkSets(Board.returnColumns(testBoard))

            assert( actual === expected)
        }

        it("should return true if there is a diagonal win") {
            val testBoard: List[Any] = List("x",2,3,4,"x",6,7,8,"x")
            val expected: Boolean = true
            val actual = Board.checkSets(Board.returnDiagonals(testBoard))

            assert(actual === expected)
        }
    }

    describe("*checkWin") {
        it("should return true if a winning board") {
            val testBoard: List[Any] = List("x","x","x",4,5,6,7,8,9)
            val expected: Boolean = true
            val actual = Board.checkWin(testBoard)

            assert(actual === expected)
        }

        it("should return false if not a winning board") {
            val testBoard: List[Any] = List(1,"x","x",4,5,6,7,8,9)
            val expected: Boolean = false 
            val actual = Board.checkWin(testBoard)

            assert(actual === expected)
        }
    }


    describe("*checkSpace") {
        it("should return true if there is open spaces") {
            val testBoard: List[Any] = List(1,2,3,4,5,6,7,8,9)
            val expected: Boolean = true
            val actual = Board.checkSpace(testBoard)

            assert(actual === expected)
        }

        it("should return false if there is no open spaces") {
            val testBoard: List[Any] = List("x","x")
            val expected = false
            val actual = Board.checkSpace(testBoard)

            assert(actual === expected)
        }
    }

    describe("*checkTie") {

        it("should return true if there is no tie") (pending)

        it("should return false if there is winner and no tie") (pending)

    }

    describe("*gameOver") {
        it("should return a List[Boolean, String]") (pending)

        it("should return false if the game is not over") (pending)

        it("should return true if the game is over") (pending)

        it("should return 'tie' if a tie") (pending)

        it("should return 'X' if X is the winner") (pending)

    }

}