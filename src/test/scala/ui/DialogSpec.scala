package tictactoe 

import org.scalatest._
import org.scalatest.Matchers._

class DialogSpec extends FunSpec {

    describe("en - english dialog") {
        it("should be a map") {
            Dialog.en.empty should be (Map())
        }

        it("should have expected keys") {
            Dialog.en should (contain key ("greeting"))
            Dialog.en should (contain key ("turnPrompt"))
            Dialog.en should (contain key ("playerAnnounce"))
            Dialog.en should (contain key ("gameOver"))
            Dialog.en should (contain key ("win"))
            Dialog.en should (contain key ("tie"))
            Dialog.en should (contain key ("invalidPlay"))
            Dialog.en should (contain key ("inputPrompt"))
        }


    }

}