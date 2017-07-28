package tictactoe

import scala.annotation.tailrec

object AI {

  type Position = Int

  class Score(p: Position, v: Int, o: String, f: Boolean) {
    val position: Position = p
    val value: Int = v
    val outcome: String = o
    val finished: Boolean = f
  }

  type Node = Map[Position, Score]
  type Depth = Int
  type NodeMap = Map[Depth, Node]

  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def generateOpenMoves(board: List[String]): List[Position] = {
    board.filter(isAllDigits(_) == true).map(x => x.toInt)
  }

  def generateNodeMap(openMoves: List[Position],
                      currentDepth: Depth,
                      previousNodeMap: NodeMap): NodeMap = {

    val nodes: List[Node] = openMoves.map { position: Position =>
      val score: Score = new Score(position, 0, "none", false)
      val node: Node = Map(position -> score)
      node
    }

    val allNodes = nodes.flatten.toMap
    Map(currentDepth -> allNodes) ++ previousNodeMap
  }

  def isDepthFinished(node: Node): Boolean = {
    node
      .keys
      .map(key => node(key).finished)
      .forall(x => true == x)
  }

  def updateBoard(board: List[String], position: Position, token: String): List[String] = {
    board.map(cell => if(cell == position.toString) token else cell)
  }

  def getActiveParentLeafPosition(nodeMap: NodeMap, depth: Depth): Position = {
    nodeMap(depth - 1)
      .filter((t) => t._2.outcome == "current")
      .keys
      .head
  }

  def rollBackBoard(board: List[String], currentDepth: Depth, nodeMap: NodeMap): List[String] = {
    val rollBackPosition: Position = getActiveParentLeafPosition(nodeMap, currentDepth - 1)
    updateBoard(board, rollBackPosition, rollBackPosition.toString)
  }

  def getLeafScore(position: Position, depth: Depth, board: List[String], maxPlayer: Boolean): Score = {
    val win: Boolean = Board.checkWin(board)
    val tie: Boolean = Board.checkTie(board)
    val outcome: String = if(win) "win" else "tie"
    val rawScore: Int = if(win) 1000 - depth else 0
    val score: Int = if(maxPlayer) rawScore else rawScore * -1
    new Score(position, score, outcome, true)
  }

  def updateScore(depth: Depth, position: Position, nodeMap: NodeMap, score: Score): NodeMap = {
    val prunedNode = nodeMap(depth) - position
    val addedNode = prunedNode + (position -> score)
    nodeMap - depth + (depth -> addedNode)
  }

  def setDepthScore(nodeMap: NodeMap, depth: Depth, maxPlayer: Boolean): NodeMap = {
    val node: Node = nodeMap(depth)
    val activeParentLeafPosition = getActiveParentLeafPosition(nodeMap, depth)

    if(maxPlayer) {
      val maxDepthScore = nodeMap(depth).keys.map(key=>nodeMap(depth)(key).value).max
      val newScore = new Score(activeParentLeafPosition, maxDepthScore, "finished", true)
      val updatedNodeMap = updateScore(depth - 1, activeParentLeafPosition, nodeMap, newScore)
      updatedNodeMap - depth
    } else {
      val minDepthScore = nodeMap(depth).keys.map(key=>nodeMap(depth)(key).value).min
      val newScore = new Score(activeParentLeafPosition, minDepthScore, "finished", true)
      val updatedNodeMap = updateScore(depth - 1, activeParentLeafPosition, nodeMap, newScore)
      updatedNodeMap - depth
    }
  }

  @tailrec def miniMax(
    boardState: List[String],
    nodeMap: NodeMap,
    depth: Depth,
    maxToken: String,
    minToken: String,
    currentToken: String): Node = {
    val allScored: Boolean = isDepthFinished(nodeMap(depth))
    if(allScored && depth == 0) {
      nodeMap(0)//base case returns map of scores
    } else if(allScored) {
      val previousBoardState = rollBackBoard(boardState, depth, nodeMap)
      val scoreDepthAndPrunedNodeMap = setDepthScore(nodeMap, depth, maxToken == currentToken)
      val changeToken = if(currentToken == maxToken) minToken else maxToken
      miniMax(previousBoardState, scoreDepthAndPrunedNodeMap, depth - 1, maxToken, minToken, currentToken)
    } else {
      val openMoves: List[Position] = generateOpenMoves(boardState)
      val position: Position = openMoves.take(1).head
      val tempBoard = updateBoard(boardState, position, currentToken)
      val tempNodeMap = updateScore(depth, position, nodeMap, new Score(position, 0, "current", false))
      val isWin: Boolean = Board.checkWin(tempBoard)
      val isTie: Boolean = Board.checkTie(tempBoard)
      if (isWin || isTie) {
        val leafScore = getLeafScore(position, depth, tempBoard, currentToken == maxToken)
        val newNodeMap = updateScore(depth, position, tempNodeMap, leafScore)
        miniMax(boardState, newNodeMap, depth, maxToken, minToken, currentToken)
      } else {
        val changeToken = if(currentToken == maxToken) minToken else maxToken
        miniMax(tempBoard, tempNodeMap, depth + 1, maxToken, minToken, changeToken)
      }
    }
  }
}