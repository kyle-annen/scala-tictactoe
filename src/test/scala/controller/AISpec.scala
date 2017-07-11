package tictactoe

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  describe("getComputerMove") {
    it("scores a two move board correctly") {
      val testBoard = List(
        "x","o","x",
        "x","o","x",
        "o","8","9")
      val expectedMove: Int = 8
      val actualMove = AI.getComputerMove(testBoard, "x", "o", "x")
      assert(actualMove === expectedMove)
    }

    it("scores a simple win correctly") {
      val testBoard = List(
        "x","o","x",
        "4","o","x",
        "7","8","9")
      val expected: Int = 8
      val actual = AI.getComputerMove(testBoard, "x", "o", "x")
      assert(actual == expected)
    }

    it("take the appropriate second move") {
      val testBoard = List(
        "o","2","3",
        "4","5","6",
        "7","8","9")
      val expected: Int = 8
      val actual = AI.getComputerMove(testBoard, "x", "o", "x")
      assert(actual == expected)
    }
  }
}
