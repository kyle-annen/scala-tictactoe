package org.clojars.kyleannen.tictactoe

object GameSetup {
  def setLanguage(
                   output: String => Any,
                   leftPadding: Int,
                   getInput: Int => String): String = {

    View.renderDialog(output, leftPadding, Dialog.lang("EN")("greeting"))
    View.renderWhitespace(output, 2)
    val langOptions = ( 1 to Dialog.lang.keys.size).toList.map(x => x.toString)
    val langKeys = Dialog.lang.keys.toList
    for(option <- langOptions) {
      View.renderDialog(output, leftPadding, option + " - " + langKeys(option.toInt - 1))
    }

    val langSelection = IO.getValidMove(
      langOptions,
      Dialog.lang("EN")("selectLang"),
      Dialog.lang("EN")("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    val langKey = langKeys(langSelection.toInt - 1)
    langKey
  }

  def setBoardSize(
                    output: String => Any,
                    leftPadding: Int,
                    getInput: Int => String,
                    dialogLang: Map[String, String]): Int = {

    View.renderDialog(output, leftPadding, dialogLang("pickBoardSize"))
    val sizeOptions = List("3", "4")
    for(option <- sizeOptions) {
      View.renderDialog(output, leftPadding, option + " - " + option + "x" + option)
    }
    val sizeSelection = IO.getValidMove(
      sizeOptions,
      dialogLang("pickBoardSize"),
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    sizeSelection.toInt
  }

  def setDifficulty(
                     output: String => Any,
                     leftPadding: Int,
                     getInput: Int => String,
                     dialogLang: Map[String, String]): Int = {

    val easy = "1 - " + dialogLang("easy")
    val medium = "2 - " + dialogLang("medium")
    val hard = "3 - " + dialogLang("hard")
    val difficultyPrompt = dialogLang("selectDifficulty")

    val prompt = difficultyPrompt +
      "\n" + " " * leftPadding + easy +
      "\n" + " " * leftPadding + medium +
      "\n" + " " * leftPadding + hard
    val validDiffs = List("1","2","3")

    val diffSelection = IO.getValidMove(
      validDiffs,
      prompt,
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)

    diffSelection match  {
      case "1" => 1
      case "2" => 2
      case "3" => 3
    }
  }

  def getDifficulty(board: List[String], difficulty: Int): Int = {
    val openSpacesCount = AI.generateOpenMoves(board).size
    val boardSize = Board.returnDimension(board)
    boardSize match {
      case 3 => difficulty match {
        case 1 => 1
        case 2 => 4
        case 3 => 10
      }
      case 4 => difficulty match {
        case 1 => 1
        case 2 => 2
        case 3 => openSpacesCount match {
          case x if x > 12 => 2
          case x if x <= 12 => 6
        }
      }
    }
  }

  def setPlayer(
                 output: String => Any,
                 leftPadding: Int,
                 getInput: Int => String,
                 dialogLang: Map[String, String],
                 playerNum: Int,
                 playerToken: String): Map[Int, (String, String, Int)] = {

    val pAnnounce = dialogLang("playerAnnounce") + playerNum.toString
    val pPrompt = dialogLang("selectPlayerType")
    val pHuman = dialogLang("pTypeHuman")
    val pComp = dialogLang("pTypeComputer")
    val prompt = pAnnounce + "\n" +  " " * leftPadding +
      pPrompt + "\n" +  " " * leftPadding +
      pHuman + "\n" + " " * leftPadding +
      pComp
    val pTypeOptions: List[String] = List("1","2")
    val playerType = IO.getValidMove(
      pTypeOptions,
      prompt,
      dialogLang("invalidPlay"),
      output,
      getInput,
      leftPadding,
      1)
    val pType = if(playerType == "1") "human" else "computer"
    val playerDifficulty = if(pType == "computer") {
      setDifficulty(output, leftPadding, getInput, dialogLang)
    } else {
      1
    }

    val player = Map(playerNum -> (pType, playerToken, playerDifficulty))
    player
  }

}
