package tictactoe

import scala.annotation.tailrec

import util.control.Breaks._

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  describe("Score") {
    it("will initilize to the valuse passed") {
      val testScore = new AI.Score(1, 987, "win", true)
      assert(testScore.position == 1)
      assert(testScore.value == 987)
      assert(testScore.outcome == "win")
      assert(testScore.finished == true)
    }
  }

  describe("isAllDigits") {
    it("will tell if a string is all digits") {
      val testString1 = "12f"
      val testString2 = "12"
      assert(AI.isAllDigits(testString1) == false)
      assert(AI.isAllDigits(testString2) == true)
    }
  }

  describe("generateOpenMoves") {
    it("returns a list of open positions") {
      val testBoard = Board.initBoard(9)
      val expected = List(1,2,3,4,5,6,7,8,9)
      val actual = AI.generateOpenMoves(testBoard)
      assert(actual == expected)
    }

    it("returns of list of open positions for a board with tokens") {
      val testBoard = List("x", "2", "3")
      val expected = List(2,3)
      val actual = AI.generateOpenMoves(testBoard)
      assert(actual == expected)
    }
  }

  describe("generateNodeMap") {
    it("generates a NodeMap with the level indicated") {
      val testBoard = Board.initBoard(9)
      val openMoves = AI.generateOpenMoves(testBoard)
      val testDepth = 0
      val previousNodeMap: AI.NodeMap = Map()
      val nodeMap = AI.generateNodeMap(openMoves, testDepth, previousNodeMap)
      val expectedKeys = Set(0)
      val expectedNodeKeys = (1 to 9).toSet
      val actualKeys = nodeMap.keys
      val actualNodeKeys = nodeMap(testDepth).keys

      assert(expectedKeys == actualKeys)
      assert(expectedNodeKeys == actualNodeKeys)
    }
  }

  


}
