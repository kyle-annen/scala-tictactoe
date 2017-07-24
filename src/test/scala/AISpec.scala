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

  describe("Score") {
    it("initiallize the passed values") {
      val testScore = new AI.Score(1, 987, "win")
      assert(testScore.position == 1)
      assert(testScore.value == 987)
      assert(testScore.outcome == "win")
    }
  }




  describe("setDepthLimit") {
    it("matches the difficulty to the board size and computer AI level") {
      assert(AI.setDepthLimit(9, "easy",0) === 1)
      assert(AI.setDepthLimit(9, "medium",0) === 3)
      assert(AI.setDepthLimit(9, "hard",0) === 10)

      assert(AI.setDepthLimit(16, "easy",0) === 1)
      assert(AI.setDepthLimit(16, "medium",0) === 3)
      assert(AI.setDepthLimit(16, "hard",0) === 6)

      assert(AI.setDepthLimit(25, "easy",0) === 1)
      assert(AI.setDepthLimit(25, "medium",0) === 3)
      assert(AI.setDepthLimit(25, "hard",0) === 5)

      assert(AI.setDepthLimit(36, "easy",0) === 1)
      assert(AI.setDepthLimit(36, "medium",0) === 3)
      assert(AI.setDepthLimit(36, "hard",0) === 4)
    }
  }


  describe("getComputerMove") {
    it("scores a two move board correctly") {
      val testBoard = List(
        "x","o","x",
        "x","o","x",
        "o","8","9")
      val expectedMove: Int = 9
      val ttTable = new TTTable.TranspositionTable
      val testParams = new AI.AIParams(testBoard, 1, "x", "o", "x", ttTable, "hard")
      val actualMove = AI.getComputerMove(testParams).position
      assert(actualMove === expectedMove)
    }

    it("scores a simple win correctly") {
      val testBoard = List(
        "x","o","x",
        "4","o","x",
        "7","8","9")
      val expected: Int = 9
      val ttTable = new TTTable.TranspositionTable
      val testParams = new AI.AIParams(testBoard, 1, "x", "o", "x", ttTable, "hard")
      val actual = AI.getComputerMove(testParams).position
      assert(actual == expected)
    }

    it("take the appropriate second move") {
      val testBoard = List(
        "o","2","3",
        "4","5","6",
        "7","8","9")
      val expected: Int = 5
      val ttTable = new TTTable.TranspositionTable
      val testParams = new AI.AIParams(testBoard, 1, "x", "o", "x", ttTable, "hard")
      val actual = AI.getComputerMove(testParams).position
      assert(actual == expected)
    }

    it("will tie given every opponent first move") {
      val openBoard = (1 to 9).toList.map(x => x.toString)
      val players = Map(1 -> ("computer", "X", "hard"), 2 -> ("computer", "O", "hard"))
      val seedBoards = openBoard.map(x => openBoard.map(cell => if(cell == x) "O" else cell ))
      val ttTable = new TTTable.TranspositionTable

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
      val ttTable = new TTTable.TranspositionTable

      def go(bState: List[String]): Unit = {
        //get the human moves
        val humOpenMoves = Board.returnValidInputs(bState)
        //populate all possible moves to the board
        for(move <- humOpenMoves) {
          breakable {
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
            val comParams = new AI.AIParams(humMoveBoard, 1, comT, humT, comT, ttTable, "hard")
            val comMove = AI.getComputerMove(comParams).position.toString
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
      val startBoard = Board.initBoard(16)
      val humT = "O"
      val comT = "X"
      val ttTable = new TTTable.TranspositionTable

      def go(bState: List[String]): Unit = {
        //get the human moves
        val humOpenMoves = Board.returnValidInputs(bState)
        //populate all possible moves to the board
        for(move <- humOpenMoves) {
          breakable {
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
            val comParams = new AI.AIParams(humMoveBoard, 1, comT, humT, comT, ttTable, "hard")
            val comMove = AI.getComputerMove(comParams).position.toString
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
    it("4x4 Computer(Hard) vs Computer(Hard) ends in ties") {
      val startBoard = Board.initBoard(16)
      val com1Token = "O"
      val com2Token = "X"
      val ttTable = new TTTable.TranspositionTable

      def go(bState: List[String]): Unit = {
        //get the human moves
        breakable {
          val com1Params = new AI.AIParams(bState, 1, com1Token, com2Token, com1Token, ttTable, "hard")
          val com1Move = AI.getComputerMove(com1Params).position.toString
          val com1MoveBoard = bState.map(cell => if(cell == com1Move) com1Token else cell)
          val com1Win = Board.checkWin(com1MoveBoard)
          val com1Tie = Board.checkTie(com1MoveBoard)
          if(com1Win) {
            println(com1MoveBoard)
            assert(com1Win == false)
            break
          }
          if(com1Tie) {
            assert(com1Tie == true)
            break
          }
          val com2Params = new AI.AIParams(com1MoveBoard, 1, com2Token, com1Token, com2Token, ttTable, "hard")
          val com2Move = AI.getComputerMove(com2Params).position.toString
          val com2MoveBoard = com1MoveBoard.map(cell => if(cell == com2Move) com2Token else cell)
          val com2Win = Board.checkWin(com2MoveBoard)
          val com2Tie = Board.checkTie(com2MoveBoard)
          if(com2Win) {
            assert(com2Win == false)
            break
          }
          if (com2Tie) {
            assert(true == true)
            break
          } else {
            go(com2MoveBoard)
          }
        }
      }
      go(startBoard)
    }
  }
}
