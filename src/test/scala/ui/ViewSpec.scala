package tictactoe

import org.scalatest.FunSpec

class ViewSpec extends FunSpec {


  def testPrint(s: String): String = s
  val testBoard3x3blank: List[String] = (1 to 9).toList.map(x=>x.toString)
  val testBoard4x4blank: List[String] = (1 to 16).toList.map(x=>x.toString)
  val testBoard5x5blank: List[String] = (1 to 25).toList.map(x=>x.toString)

  describe("renderWhitespace") {
    it("renders n number of linebreaks") {
      val expected = "\n\n"
      val actual = View.renderWhitespace(testPrint, 2)

      assert(actual === expected)
    }
  }

  describe("renderDialog") {
    it("renders dialog of one string without line breaks") {
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

    it("each message renders on own line") {
      val expected = "1\n2\n3"
      val actual = View.renderDialog(testPrint, 0, "1", "2", "3")

      assert(actual === expected)
    }
  }

  describe("formatBoard") {
    it("formats a normal board into groups") {
      val expected = List(
        List("1","2","3"),
        List("4","5","6"),
        List("7","8","9")
      )
      val actual = View.formatBoard(testBoard3x3blank)
      assert(actual == expected)
    }

    it("format a large board into groups") {
      val expected = List(
        List("1","2","3","4","5"),
        List("6","7","8","9","10"),
        List("11","12","13","14","15"),
        List("16","17","18","19","20"),
        List("21","22","23","24","25")
      )
      val actual = View.formatBoard(testBoard5x5blank)
      assert(actual == expected)
    }
  }

  describe("formatRow") {
    it("formats the row of the board") {
      val testRow = List("1","2","X")
      val expected = " 1 | 2 | X "
      val actual = View.formatRow(testRow,3)
      assert(actual === expected)
    }
  }

  describe("renderBoard") {
    it("renders a 3x3 board") {
      val expected = "\n 1 | 2 | 3 \n===+===+===\n 4 | 5 | 6 \n===+===+===\n 7 | 8 | 9 \n"
      val fBoard = View.formatBoard(testBoard3x3blank)
      val actual = View.renderBoard(testPrint,fBoard,0)
      assert(actual === expected)
    }

    it("renders a 4x4 board") {
      val expected =
        "\n  1  |  2  |  3  |  4  \n=====+=====+=====+=====" +
        "\n  5  |  6  |  7  |  8  \n=====+=====+=====+=====" +
        "\n  9  |  10 |  11 |  12 \n=====+=====+=====+=====" +
        "\n  13 |  14 |  15 |  16 \n"
      val fBoard = View.formatBoard(testBoard4x4blank)
      val actual = View.renderBoard(testPrint,fBoard,0)
      assert(actual === expected)
    }
  }
}
