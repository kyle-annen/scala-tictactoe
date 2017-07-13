package tictactoe

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class DialogSpec extends FunSpec {
  describe("lang") {
    it("should be a map") {
      Dialog.lang.empty should be (Map())
    }

    it("has expected languages") {
      Dialog.lang should (contain key ("EN"))
      Dialog.lang should (contain key ("CN"))
    }

    describe("EN") {
      it("has expected dialog") {
        Dialog.lang("EN") should (contain key ("greeting"))
        Dialog.lang("EN") should (contain key ("selectLang"))
        Dialog.lang("EN") should (contain key ("turnPrompt"))
        Dialog.lang("EN") should (contain key ("playerAnnounce"))
        Dialog.lang("EN") should (contain key ("gameOver"))
        Dialog.lang("EN") should (contain key ("win"))
        Dialog.lang("EN") should (contain key ("tie"))
        Dialog.lang("EN") should (contain key ("invalidPlay"))
        Dialog.lang("EN") should (contain key ("inputPrompt"))
        Dialog.lang("EN") should (contain key ("pickBoardSize"))
      }
    }

    describe("CN") {
      it("has expected dialog") {
        Dialog.lang("CN") should (contain key ("greeting"))
        Dialog.lang("CN") should (contain key ("selectLang"))
        Dialog.lang("CN") should (contain key ("turnPrompt"))
        Dialog.lang("CN") should (contain key ("playerAnnounce"))
        Dialog.lang("CN") should (contain key ("gameOver"))
        Dialog.lang("CN") should (contain key ("win"))
        Dialog.lang("CN") should (contain key ("tie"))
        Dialog.lang("CN") should (contain key ("invalidPlay"))
        Dialog.lang("CN") should (contain key ("inputPrompt"))
        Dialog.lang("EN") should (contain key ("pickBoardSize"))
      }
    }
  }
}
