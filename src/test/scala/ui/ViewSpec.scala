package view 

import org.scalatest._

class ViewSpec extends FunSpec {

  describe("View") {
    describe("greeting") {
      it("Should have defualt greeting") {
        assert(View.greeting == "Welcome to TicTacToe")
      }
    }

    describe("render") {
      it("should be defined") {
        assert()
      }
    }
  }

}
