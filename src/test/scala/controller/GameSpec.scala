package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class GameSpec extends FunSpec {

    describe("initPlayers") {
        it("should return an Map[Int,String] of the players") {
            val actual = Game.initPlayers()
            val expected: Map[Int, String] = Map(1-> "X", 2->"O")

            assert(actual === expected)
        }
    }

}