package tictactoe

import scala.annotation.tailrec

import util.control.Breaks._

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  val testBoardGeneral = Board.initBoard(9)
  val openMovesGeneral = AI.generateOpenMoves(testBoardGeneral)
  val testDepthGeneral = 0
  val previousNodeMapGeneral: AI.NodeMap = Map()
  val nodeMapGeneral = AI.generateNodeMap(openMovesGeneral, testDepthGeneral, previousNodeMapGeneral)

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

  describe("isDepthFinished") {
    it("returns false if scores are not populated") {
      val expected = false
      val actual = AI.isDepthFinished(nodeMapGeneral(0))
      assert(actual == expected)
    }

    it("returns true if scores are finished") {
      val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 987, "win", true),
          2 -> new AI.Score(2, -987, "win", true)
        )
      )
      val expected = true
      val actual = AI.isDepthFinished(testNodeMap(0))
      assert(actual == expected)
    }
  }

  describe("updateBoard") {
    it("returns an updated board") {
      val testBoard = List("1","2","3")
      val expectedBoard = List("1","X","3")
      val actualBoard = AI.updateBoard(testBoard, 2, "X")
      assert(actualBoard == expectedBoard)
    }
  }

  describe("getActiveParentLeafPosition") {
    it("returns the position of the active node one level up") {
      val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 987, "win", true),
          2 -> new AI.Score(2, -987, "current", false),
        ),
        1 -> Map()
      )

      val expected = 2
      val actual = AI.getActiveParentLeafPosition(testNodeMap, 1)
      assert(actual == expected)
    }
  }

  describe("rollBackBoard") {
    it("rolls the board back to the previous level") {
      val testBoard = List("1","O","X")
      val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 987, "win", true),
          2 -> new AI.Score(2, -987, "current", false),
        ),
        1 -> Map()
      )
      val expected = List("1","2","X")
      val actual = AI.rollBackBoard(testBoard, 1, testNodeMap)
      assert(actual == expected)
    }
  }




}


























