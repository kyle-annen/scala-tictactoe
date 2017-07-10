package tictactoe

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  describe("minimax") {
    it("scores a two move board correctly") {
      val testBoard = List(
        "x", "o", "x",
        "x", "o", "x",
        "o", "8", "9")
      val expectedMove: Int = 8 
      val actualMove = AI.getComputerMove(testBoard, "x", "o", "x")

      assert(actualMove === expectedMove)
    }
  }

}
