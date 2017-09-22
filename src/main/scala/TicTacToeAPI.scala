package tictactoe

object TicTacToeAPI {

  def startGame: Map[Symbol, String] = {
    Map(
      "board" -> "1,2,3,4,5,6,7,8,9",
      "messages" -> Dialog.lang("EN")
    )
  }

  def submitParams(params: String): GameState = {
    val paramMap: Map[String, String] = parseInput(params)
    val board = validateParam("board", paramMap)
    val humanMove = validateParam("move", paramMap)
  }




  def playRound(gameState: GameState): GameState = {
    gameState.progressGameState()
  }


  def validateParam(key: String, paramMap: Map[String, String]): String = {
    if(paramMap.keySet.contains(key)) paramMap(key) else "Error"
  }

  def parseInput(params: String): Map[String, String] = {
    val paramArray: Array[String] = params.split('&')
    val paramMap: Map[String, String] =
      paramArray
        .map( x => x.split('='))
        .toList
        .flatten
        .grouped(2)
        .collect {
          case List(k, v) => k -> v
        }.toMap
    paramMap
  }



}


