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
          2 -> new AI.Score(2, -987, "current", false)
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
          2 -> new AI.Score(2, -987, "current", false)
        ),
        1 -> Map()
      )
      val expected = List("1","2","X")
      val actual = AI.rollBackBoard(testBoard, 1, testNodeMap)
      assert(actual == expected)
    }
  }

  describe("getLeafScore") {
    it("returns a score of a terminal node") {
      val testBoard = List("X","X","X","3","4","5","6","7","8","9")
      val expected = new AI.Score(1, -997, "win", true)
      val actual = AI.getLeafScore(1, 3, testBoard, false)
      assert(actual.position == expected.position)
      assert(actual.value == expected.value)
      assert(actual.outcome == expected.outcome)
      assert(actual.finished == expected.finished)
    }
  }

  describe("updateScore") {
    it("updates a score in the nodeMap") {
      val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 987, "win", true),
          2 -> new AI.Score(2, -987, "current", false)
        ),
        1 -> Map()
      )
      val updateDepth = 0
      val updatePosition = 1
      val updateScore = new AI.Score(1, 999, "win", true)

      val actual = AI.updateScore(updateDepth, updatePosition, testNodeMap, updateScore)

      assert(actual(0)(1).position == updatePosition)
      assert(actual(0)(1).value == 999)
      assert(actual(0)(1).outcome == "win")
      assert(actual(0)(1).finished == true)
    }
  }

  describe("setDepthScore") {
    it("sets the heuristic value of child nodes to the parent leaf") {
      val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 0, "current", false),
          2 -> new AI.Score(2, 0, "none", false)
        ),
        1 -> Map(
          2 -> new AI.Score(2, 999, "win", true)
        )
      )

      val actual = AI.setDepthScore(testNodeMap, 1, true)
      assert(actual(0)(1).value == 999)
    }
  }

  describe("getFirstOpenPosition") {
    it("returns the first open position in a node") {
       val testNodeMap: AI.NodeMap = Map(
        0 -> Map(
          1 -> new AI.Score(1, 0, "current", false),
          2 -> new AI.Score(2, 0, "none", false),
          3 -> new AI.Score(3, 0, "none", false)
        ),
        1 -> Map(
          2 -> new AI.Score(2, 999, "win", true),
          3 -> new AI.Score(3, 987, "win", true)
        )
      )
      val expected = 1
      val actual = AI.getFirstOpenPosition(testNodeMap, 0)
      assert(actual == expected)
    }
  }

  describe("negaMax") {
    it("returns scores for an empty board") {
      val testBoard = Board.initBoard(9)
      val negaMaxResult = AI.negaMax(testBoard, Map(0 -> Map()), 0, "X","O","X", 10)
      assert(negaMaxResult > 0)

    }

    it("will block a next move win") {
      val testBoard = List("O","O","3","X","5","6","7","8","9")
      val negaMaxResult = AI.negaMax(testBoard, Map(0-> Map()), 0, "X","O","X", 10)
      assert(negaMaxResult == 3)
    }

    it("will block a next move win on 4x4") {
      val testBoard = List(
        "O", "X", "O", "X",
        "X", "O", "O", "8",
        "9", "10","11","12",
        "13","14","O","16")
      val negaMaxResult = AI.negaMax(testBoard, Map(0-> Map()), 0, "X","O","X", 6)
      assert(negaMaxResult == 11)
    }

    it("wins or ties in every possible situation on a 3x3 board") {
      var combos: Int = 0
      val startBoard = Board.initBoard(9)
      def go(board: List[String]) {
        val openMoves = AI.generateOpenMoves(board)
        openMoves.foreach { move =>
          breakable {
            val humanMoveBoard = board.map(x => if(move.toString == x) "O" else x)
            combos += 1
            val humanWin = Board.checkWin(humanMoveBoard)
            val humanTie = Board.checkTie(humanMoveBoard)
            if(humanWin) {
              assert(false)
              break
            } else if(humanTie) {
              assert(true)
              break
            } else {
              val compMove = AI.negaMax(humanMoveBoard, Map(0-> Map()), 0, "X","O","X", 10)
              combos += 1
              val compMoveBoard = humanMoveBoard.map(x => if(compMove.toString == x) "X" else x)
              val compWin = Board.checkWin(compMoveBoard)
              val compTie = Board.checkTie(compMoveBoard)
              if(compWin || compTie) {
                assert(true)
                break
              } else {
                go(compMoveBoard)
              }
            }

          }
        }
      }
      go(startBoard)
    }
  }
}



























