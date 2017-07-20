package tictactoe

import scala.annotation.tailrec

import util.control.Breaks._

import org.scalatest.FunSpec

class AISpec extends FunSpec {

  def testPrint(s: String): String = s

  describe("AlphaBeta") {
    describe("alpha") {
      it("initializes as Negative Infinity") {
        val ab = new AI.AlphaBeta
        val expected = Double.NegativeInfinity
        val actual = ab.alpha
        assert(actual == expected)
      }
    }

    describe("beta") {
      it("initializes as Positive Infinity") {
        val ab = new AI.AlphaBeta
        val expected = Double.PositiveInfinity
        val actual = ab.beta
        assert(actual == expected)
      }
    }
  }

  describe("TranspositionTable") {
    describe("min") {
      it("a value added can be retrieved with key") {
        val ttTable = new AI.TranspositionTable
        ttTable.min += ("123" -> -987)
        val expected = -987
        val actual = ttTable.min("123")
        assert(actual == expected)
      }
    }

    describe("max") {
      it("a value added can be retrieved with key") {
        val ttTable = new AI.TranspositionTable
        ttTable.max += ("123" -> 987)
        val expected = 987
        val actual = ttTable.max("123")
        assert(actual == expected)
      }
    }
  }

  describe("getBoardTranspositions") {
    it("give scores of each 90 degree rotation of board") {
      val testBoard = List("1","2","3","4","5","6","X","O","X")
      val expected = List(
        ("------XOX", 987),
        ("--X--O--X", 987),
        ("XOX------", 987),
        ("X--O--X--", 987))
      val actual = AI.getBoardTranspositions(testBoard, 987, "X","O")
      assert(actual == expected)
    }
  }

  describe("saveTranspositions") {
    it("saves a transposition list to a transposition table") {
      val testBoard = List("1","2","3","4","5","6","X","O","X")
      val ttTable = new AI.TranspositionTable
      val values = AI.getBoardTranspositions(testBoard, 987, "X","O")
      AI.saveTranspositions(ttTable, values, "max")
      assert(ttTable.max.contains("------XOX"))
      assert(ttTable.max.contains("XOX------"))
      assert(ttTable.max.contains("--X--O--X"))
      assert(ttTable.max.contains("X--O--X--"))
    }
  }

  describe("checkTransposition") {
    it("will tell if transposition does not exist") {
      val testBoard = List("1","2","3","4","5","6","X","O","X")
      val ttTable = new AI.TranspositionTable
      val values = AI.getBoardTranspositions(testBoard, 987, "X","O")
      AI.saveTranspositions(ttTable, values, "max")
      val testCheckBoard = List("X","2","3","4","5","6","X","O","X")
      val expected = (false -> 0)
      val actual = AI.checkTransposition(testCheckBoard, ttTable, "X","O", "max")
      assert(actual == expected)
    }
  }


  describe("setDepthLimit") {
    it("matches the difficulty to the board size and computer AI level") {
      assert(AI.setDepthLimit(9, "easy") === 1)
      assert(AI.setDepthLimit(9, "medium") === 3)
      assert(AI.setDepthLimit(9, "hard") === 100)

      assert(AI.setDepthLimit(16, "easy") === 1)
      assert(AI.setDepthLimit(16, "medium") === 3)
      assert(AI.setDepthLimit(16, "hard") === 6)

      assert(AI.setDepthLimit(25, "easy") === 1)
      assert(AI.setDepthLimit(25, "medium") === 3)
      assert(AI.setDepthLimit(25, "hard") === 5)

      assert(AI.setDepthLimit(36, "easy") === 1)
      assert(AI.setDepthLimit(36, "medium") === 3)
      assert(AI.setDepthLimit(36, "hard") === 4)
    }
  }


  describe("getComputerMove") {
    it("scores a two move board correctly") {
      val testBoard = List(
        "x","o","x",
        "x","o","x",
        "o","8","9")
      val expectedMove: Int = 8
      val ttTable = new AI.TranspositionTable
      val actualMove = AI.getComputerMove(testBoard, "x", "o", "x", ttTable, "hard")
      assert(actualMove === expectedMove)
    }

    it("scores a simple win correctly") {
      val testBoard = List(
        "x","o","x",
        "4","o","x",
        "7","8","9")
      val expected: Int = 8
      val ttTable = new AI.TranspositionTable
      val actual = AI.getComputerMove(testBoard, "x", "o", "x", ttTable, "hard")
      assert(actual == expected)
    }

    it("take the appropriate second move") {
      val testBoard = List(
        "o","2","3",
        "4","5","6",
        "7","8","9")
      val expected: Int = 4
      val ttTable = new AI.TranspositionTable
      val actual = AI.getComputerMove(testBoard, "x", "o", "x", ttTable, "hard")
      assert(actual == expected)
    }
  }
  it("will tie given every opponent first move") {
    val openBoard = (1 to 9).toList.map(x => x.toString)
    val players = Map(1 -> ("computer", "X", "hard"), 2 -> ("computer", "O", "hard"))
    val seedBoards = openBoard.map(x => openBoard.map(cell => if(cell == x) "O" else cell ))
    val ttTable = new AI.TranspositionTable

    for(board <- seedBoards) {
      val actual = Game.go(board, players, Dialog.lang("EN"), false, 1, testPrint, 0, 0, IO.getInput, 1, ttTable)
      val expected = Map(2 -> false)

      assert(actual === expected)
    }
  }

  it("will win or tie in all possible situations (3x3 board)") {
    val startBoard = (1 to 9).toList.map(x=>x.toString)
    val humT = "O"
    val comT = "X"
    val ttTable = new AI.TranspositionTable

    def go(bState: List[String]): Unit = {
      //get the human moves
      val humOpenMoves = Board.returnValidInputs(bState)
      //populate all possible moves to the board
      breakable {
        for(move <- humOpenMoves) {
          val humMoveBoard = bState.map(cell => if(cell == move) humT else cell)
          val humWin = Board.checkWin(humMoveBoard)
          val humTie = Board.checkTie(humMoveBoard)
          if(humWin) {
            println(humMoveBoard)
            assert(humWin == false)
            break
          }
          if(humTie) {
            assert(humTie == true)
            break
          }
          val comMove = (AI.getComputerMove(humMoveBoard, comT, humT, comT, ttTable, "hard") + 1).toString
          val comBoard = humMoveBoard.map(cell => if(cell == comMove) comT else cell)
          val comWin = Board.checkWin(comBoard)
          val comTie = Board.checkTie(comBoard)
          if (comWin || comTie) {
            assert(true == true)
            break
          } else {
            go(comBoard)
          }
        }
      }
    }
    go(startBoard)
  }

  it("will win or tie in all possible situations (4x4 board)") {
    val startBoard = (1 to 16).toList.map(x=>x.toString)
    val humT = "O"
    val comT = "X"
    val ttTable = new AI.TranspositionTable

    def go(bState: List[String]): Unit = {
      //get the human moves
      val humOpenMoves = Board.returnValidInputs(bState)
      //populate all possible moves to the board
      breakable {
        for(move <- humOpenMoves) {
          val humMoveBoard = bState.map(cell => if(cell == move) humT else cell)
          val humWin = Board.checkWin(humMoveBoard)
          val humTie = Board.checkTie(humMoveBoard)
          if(humWin) {
            println(humMoveBoard)
            assert(humWin == false)
            break
          }
          if(humTie) {
            assert(humTie == true)
            break
          }
          val comMove = (AI.getComputerMove(humMoveBoard, comT, humT, comT, ttTable, "hard") + 1).toString
          val comBoard = humMoveBoard.map(cell => if(cell == comMove) comT else cell)
          val comWin = Board.checkWin(comBoard)
          val comTie = Board.checkTie(comBoard)
          if (comWin || comTie) {
            assert(true == true)
            break
          } else {
            go(comBoard)
          }
        }
      }
    }
    go(startBoard)
  }

}
