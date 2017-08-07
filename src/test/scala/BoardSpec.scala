package tictactoe 
import org.scalatest._
import org.scalatest.Matchers._

class BoardSpec extends FunSpec {

  describe("initBoard") {
    it("board can be created in 3x3 grid") {
      val expected: List[String] = (1 to 9).toList.map(x => x.toString)
      val actual = Board.initBoard(9)
      assert(actual === expected)
    }

    it("board can be created in 4x4 grid") {
      val expected: List[String] = (1 to 16).toList.map(x => x.toString)
      val actual = Board.initBoard(16)
      assert(actual === expected)
    }

    it("board can be created in 5x5 grid") {
      val expected: List[String] = (1 to 25).toList.map(x => x.toString)
      val actual = Board.initBoard(25)
      assert(actual === expected)
    }

    it("board can be created in 6x6 grid") {
      val expected: List[String] = (1 to 36).toList.map(x => x.toString)
      val actual = Board.initBoard(36)
      assert(actual === expected)
    }
  }

  describe("returnValidInputs") {
    it("returns open spaces on the board") {
      val testBoard: List[String] = List(
        "x","o","x",
        "3","4","5",
        "x","o","x")
      val expected: List[String] = List("3","4","5")
      val actual = Board.returnValidInputs(testBoard)
      assert( actual === expected )
    }
  }

  describe("returnRows") {
    it("returns the rows of the board") {
      val testBoard: List[String] = (1 to 9).toList.map(x => x.toString)
      val expected: List[List[String]] = List(
        List("1","2","3"),
        List("4","5","6"),
        List("7","8","9"))
      val actual: List[List[String]] = Board.returnRows(testBoard)
      assert( actual === expected )
    }

    it("returns the rows of a 4x4 board") {
      val testBoard: List[String] = Board.initBoard(16)
      val expected: List[List[String]] = List(
        List("1","2","3","4"),
        List("5","6","7","8"),
        List("9","10","11","12"),
        List("13","14","15","16"))
      val actual = Board.returnRows(testBoard)
      assert(actual == expected)
    }
  }

  describe("returnColumns") {
    it("returns the columns of the board") {
      val testBoard: List[String] = (1 to 9).toList.map(x => x.toString)
      val expected: List[List[String]] = List(
        List("1","4","7"),
        List("2","5","8"),
        List("3","6","9"))
      val actual: List[List[String]] = Board.returnColumns(testBoard)
      assert( actual === expected )
    }

    it("returnts the colums of a 4x4 board") {
      val testBoard: List[String] = Board.initBoard(16)
      val expected: List[List[String]] = List(
        List("1","5","9","13"),
        List("2","6","10","14"),
        List("3","7","11","15"),
        List("4","8","12","16"))
      val actual = Board.returnColumns(testBoard)
      assert(actual == expected)
    }
  }

  describe("returnDiagonals") {
    it("returns the diagonals of the board") {
      val testBoard: List[String] = Board.initBoard(9)
      val expected: List[List[String]] = List(
        List("1","5","9"),
        List("3","5","7"))
      val actual: List[List[String]] = Board.returnDiagonals(testBoard)
      assert( actual === expected )
    }

    it("return diagonals of a 4x4 board") {
      val testBoard = Board.initBoard(16)
      val expected = List(
        List("1","6","11","16"),
        List("4","7","10","13"))
      val actual = Board.returnDiagonals(testBoard)
      assert(actual == expected)
    }
  }

  describe("checkSets") {
    it("detects if a row/column/diagonal is a winning combo") {
      val testBoard: List[String] = List(
        "x","x","x",
        "4","5","6",
        "7","8","9")
      val expected: Boolean = true
      val actual = Board.checkSets(Board.returnRows(testBoard))
      assert( actual === expected)
    }

    it("detects if no row/column/diagonal has winning combo") {
      val testBoard: List[String] = List(
        "x","x","x",
        "4","5","6",
        "7","8","9")
      val expected: Boolean = false
      val actual = Board.checkSets(Board.returnColumns(testBoard))
      assert( actual === expected)
    }

    it("detects if a diagonal winning combo") {
      val testBoard: List[String] = List(
        "x","2","3",
        "4","x","6",
        "7","8","x")
      val expected: Boolean = true
      val actual = Board.checkSets(Board.returnDiagonals(testBoard))
      assert(actual === expected)
    }
  }

  describe("checkWin") {
    it("detects a winning board") {
      val testBoard: List[String] = List(
        "x","x","x",
        "4","5","6",
        "7","8","9")
      val expected: Boolean = true
      val actual = Board.checkWin(testBoard)
      assert(actual === expected)
    }

    it("detects a non-winning board") {
      val testBoard: List[String] = List(
        "1","x","x",
        "4","5","6",
        "7","8","9")
      val expected: Boolean = false
      val actual = Board.checkWin(testBoard)
      assert(actual === expected)
    }

    it("detects a non-winning board on 4x4") {
      val testBoard: List[String] = List(
        "X","X","X","4",
        "5","O","O","8",
        "X","X","X","O",
        "O","O","15","O")
      val expected: Boolean = false
      val actual = Board.checkWin(testBoard)
      assert(actual === expected)

    }
  }

  describe("checkSpace") {
    it("detects if there are open spaces on the board") {
      val testBoard: List[String] = Board.initBoard(9)
      val expected: Boolean = true
      val actual = Board.checkSpace(testBoard)
      assert(actual === expected)
    }

    it("detects if there are no spaces open on the board") {
      val testBoard: List[String] = List("x","x")
      val expected = false
      val actual = Board.checkSpace(testBoard)
      assert(actual === expected)
    }
  }

  describe("checkTie") {
    it("determins not tie if board is blank") {
      val testBoard: List[String] = Board.initBoard(9)
      val expected: Boolean = false
      val actual = Board.checkTie(testBoard)
      assert(actual === expected)
    }

    it("detects no tie if moves are open") {
      val testBoard: List[String] = List(
        "x","o","x",
        "x","o","x",
        "o","x","9")
      val expected: Boolean = false
      val actual = Board.checkTie(testBoard)
      assert(actual === expected)
    }

    it("detects tie if no spaces are left and no winner") {
      val testBoard: List[String] = List(
        "x","o","x",
        "x","o","x",
        "o","x","o")
      val expected: Boolean = true
      val actual = Board.checkTie(testBoard)
      assert(actual === expected)
    }

    it("detects a tie if no winner and no open moves") {
      val testBoard: List[String] = List(
        "x","o","x",
        "x","o","x",
        "o","x","x")
      val expected: Boolean = true
      val actual = Board.checkTie(testBoard)
      assert(actual === expected)
    }
  }

  describe("gameOver") {
    it("game is over if tie and no moves open") {
      val testBoard: List[String] = List(
        "x","o","x",
        "x","o","x",
        "o","x","o")
      val expected: Boolean = true
      val actual = Board.gameOver(testBoard)
      assert(actual === expected)
    }

    it("no game over if spaces are open and no winner") {
      val testBoard: List[String] = List(
        "x","o","x",
        "x","o","x",
        "o","x","9")
      val expected: Boolean = false
      val actual = Board.gameOver(testBoard)
      assert(actual === expected)
    }

    it("game over if there is a winning combo") {
      val testBoard: List[String] = List(
        "x","x","x",
        "4","5","6",
        "7","8","9")
      val expected: Boolean = true
      val actual = Board.gameOver(testBoard)
      assert(actual === expected)
    }
  }

  describe("getWinner") {
    it("can identify the winner's token") {
      val testBoard: List[String] = List(
        "x","x","x",
        "4","5","6",
        "7","8","9")
      val expected: String = "x"
      val actual = Board.getWinner(testBoard)
      assert(actual === expected)
    }

    it("can identify if there is no winner") {
      val testBoard: List[String] = List(
        "1","2","3",
        "4","x","6",
        "7","8","9")
      val expected: String = "none"
      val actual = Board.getWinner(testBoard)
      assert(actual === expected)
    }
  }


}
