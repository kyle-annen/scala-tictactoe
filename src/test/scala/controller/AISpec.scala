package tictactoe

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  describe("minimax") {
    it("scores a two move board correctly") {
      val testBoard = List(
        "x", "o", "x",
        "x", "o", "x",
        "o", "8", "9")
      val expectedMove = List("9", "x")
      val actualMove = AI.minimax(testboard, "x")

      assert(actualMove === expectedMove)
    }
  }

}
