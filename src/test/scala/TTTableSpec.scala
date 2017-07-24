package tictactoe

import org.scalatest.FunSpec

class TTTableSpec extends FunSpec {
  describe("TranspositionTable") {
    describe("min") {
      it("a value added can be retrieved with key") {
        val ttTable = new TTTable.TranspositionTable
        ttTable.min += ("123" -> -987)
        val expected = -987
        val actual = ttTable.min("123")
        assert(actual == expected)
      }
    }

    describe("max") {
      it("a value added can be retrieved with key") {
        val ttTable = new TTTable.TranspositionTable
        ttTable.max += ("123" -> 987)
        val expected = 987
        val actual = ttTable.max("123")
        assert(actual == expected)
      }
    }

    describe("swapTranspositionKeys") {
      it("give the key for same board state with tokens swapped") {
        val testKey = "---XOX---"
        val expected = "---OXO---"
        val actual = TTTable.swapTranspositionKeys(testKey, "X","O")
        assert(actual == expected)
      }
    }

    describe("getBoardTranspositions") {
      it("give scores of each 90 degree rotation of board") {
        val testBoard = List("1","2","3","4","5","6","X","O","X")
        val expectedCurrent = List(
          ("------XOX", 987),
          ("--X--O--X", 987),
          ("XOX------", 987),
          ("X--O--X--", 987))
        val expectedOpposite = List(
          ("------OXO", -987),
          ("--O--X--O", -987),
          ("OXO------", -987),
          ("O--X--O--", -987))
        val actual = TTTable.getBoardTranspositions(testBoard, 987, "X","O")
        assert(actual("current") == expectedCurrent)
        assert(actual("opposite") == expectedOpposite)
      }
    }

    describe("saveTranspositions") {
      it("saves a transposition list to a transposition table") {
        val testBoard = List("1","2","3","4","5","6","X","O","X")
        val ttTable = new TTTable.TranspositionTable
        val values = TTTable.getBoardTranspositions(testBoard, 987, "X","O")
        val currentValues = values("current")
        TTTable.saveTranspositions(ttTable, values, "max")
        assert(ttTable.max.contains("------XOX"))
        assert(ttTable.max.contains("XOX------"))
        assert(ttTable.max.contains("--X--O--X"))
        assert(ttTable.max.contains("X--O--X--"))
      }
    }

    describe("checkTransposition") {
      it("will tell if transposition does not exist") {
        val testBoard = List("1","2","3","4","5","6","X","O","X")
        val ttTable = new TTTable.TranspositionTable
        val values = TTTable.getBoardTranspositions(testBoard, 987, "X","O")
        TTTable.saveTranspositions(ttTable, values, "max")
        val testCheckBoard = List("X","2","3","4","5","6","X","O","X")
        val expected = (false -> 0)
        val actual = TTTable.checkTransposition(testCheckBoard, ttTable, "X","O", "max")
        assert(actual == expected)
      }
    }
  }
}
