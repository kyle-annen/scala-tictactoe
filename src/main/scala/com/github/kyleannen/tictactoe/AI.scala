package com.github.kyleannen.tictactoe

import scala.annotation.tailrec
import scala.util.Random

object AI {

  var bestMovesNode: Any = 0
  type Position = Int

  class Score(p: Position, v: Int, o: String, f: Boolean) {
    val position: Position = p
    val value: Int = v
    val outcome: String = o
    val finished: Boolean = f
  }

  type Node = Map[Position, Score]
  type NodeMap = Map[Int, Node]

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  def isAllDigits(x: String): Boolean = x forall Character.isDigit

  def generateOpenMoves(board: List[String]): List[Position] = {
    board.filter(isAllDigits(_) == true).map(x => x.toInt)
  }

  def generateNodeMap(
    openMoves: List[Position],
    currentDepth: Int,
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

  def getActiveParentLeafPosition(nodeMap: NodeMap, depth: Int): Position = {
    nodeMap(depth - 1)
      .filter((t) => t._2.outcome == "current")
      .keys
      .head
  }

  def rollBackBoard(board: List[String], currentDepth: Int, nodeMap: NodeMap): List[String] = {
    val rollBackPosition: Position = getActiveParentLeafPosition(nodeMap, currentDepth)
    val newBoard: List[String] = (0 to board.length - 1).map {
      loc => if(loc == rollBackPosition - 1) rollBackPosition.toString else board(loc)
    }.toList
    newBoard
  }

  def getLeafScore(position: Position, depth: Int, board: List[String], maxPlayer: Boolean): Score = {
    val win: Boolean = Board.checkWin(board)
    val tie: Boolean = Board.checkTie(board)
    val outcome: String = if(win) "win" else "tie"
    val rawScore: Int = if(win) (1000 - depth) else 0
    val score: Int = if(maxPlayer) rawScore else rawScore * -1
    new Score(position, score, outcome, true)
  }

  def updateScore(depth: Int, position: Position, nodeMap: NodeMap, score: Score): NodeMap = {
    val prunedNode = nodeMap(depth) - position
    val addedNode = prunedNode + (position -> score)
    nodeMap - depth + (depth -> addedNode)
  }

  def setDepthScore(nodeMap: NodeMap, depth: Int, maxPlayer: Boolean): NodeMap = {
    val node: Node = nodeMap(depth)
    val activeParentLeafPosition = getActiveParentLeafPosition(nodeMap, depth)

    if(maxPlayer) {
      val maxScore = nodeMap(depth).maxBy(_._2.value)
      val maxDepthScore = maxScore._2.value
      val maxOutcome = maxScore._2.outcome
      val newScore = new Score(activeParentLeafPosition, maxDepthScore, maxOutcome, true)
      val updatedNodeMap = updateScore(depth - 1, activeParentLeafPosition, nodeMap, newScore)
      updatedNodeMap - depth
    } else {
      val minScore = nodeMap(depth).minBy(_._2.value)
      val minDepthScore = minScore._2.value
      val minOutcome = minScore._2.outcome
      val newScore = new Score(activeParentLeafPosition, minDepthScore, minOutcome, true)
      val updatedNodeMap = updateScore(depth - 1, activeParentLeafPosition, nodeMap, newScore)
      updatedNodeMap - depth
    }
  }

  def getFirstOpenPosition(nodeMap: NodeMap, depth: Int): Position = {
    nodeMap(depth).filter(_._2.finished == false).keys.head
  }

  @tailrec def negaMax(
    boardState: List[String],
    nodeMap: NodeMap,
    depth: Int,
    maxToken: String,
    minToken: String,
    currentToken: String,
    depthLimit: Int): Position = {
    val allScored: Boolean = isDepthFinished(nodeMap(depth))
    if(nodeMap(0).isEmpty) {
      val newOpenMoves = generateOpenMoves(boardState)
      val newNodeMap = generateNodeMap(newOpenMoves,0,Map())
      negaMax(boardState,newNodeMap,depth,maxToken, minToken, currentToken, depthLimit)
    } else if(allScored && depth == 0) {
      val bestScore = nodeMap(0).maxBy(_._2.value)._2.value
      val bestMoves = nodeMap(0).filter((x) => x._2.value == bestScore).keys.toList
      val randomBestMove = Random.shuffle(bestMoves).head
      randomBestMove
    } else if(allScored) {
      val previousBoardState = rollBackBoard(boardState, depth, nodeMap)
      val scoreDepthAndPrunedNodeMap = setDepthScore(nodeMap, depth, maxToken == currentToken)
      val changeToken = if(currentToken == maxToken) minToken else maxToken
      negaMax(previousBoardState, scoreDepthAndPrunedNodeMap, depth - 1, maxToken, minToken, changeToken, depthLimit)
    } else {
      if (depth >= depthLimit) {
        val position: Position = getFirstOpenPosition(nodeMap, depth)
        val rawScore: Int = 1000 - depth.toInt - 2
        val depthLimitScore: Int = if(maxToken == currentToken) rawScore else rawScore * -1
        val depthScore: Score = new Score(position, depthLimitScore, "depthLimit", true)
        val depthLimitNodeMap = updateScore(depth, position, nodeMap, depthScore)
        negaMax(boardState, depthLimitNodeMap, depth, maxToken, minToken, currentToken, depthLimit)
      } else {
        val position: Position = getFirstOpenPosition(nodeMap, depth)
        val tempBoard = updateBoard(boardState, position, currentToken)
        val tempNodeMap = updateScore(depth, position, nodeMap, new Score(position, 0, "current", false))
        val isWin: Boolean = Board.checkWin(tempBoard)
        val isTie: Boolean = Board.checkTie(tempBoard)
        if (isWin || isTie) {
          val leafScore = getLeafScore(position, depth, tempBoard, currentToken == maxToken)
          val newNodeMap = updateScore(depth, position, tempNodeMap, leafScore)
          negaMax(boardState, newNodeMap, depth, maxToken, minToken, currentToken, depthLimit)
        } else {
          val changeToken = if(currentToken == maxToken) minToken else maxToken
          val deeperOpenMoves = generateOpenMoves(tempBoard)
          val deeperNodeMap = generateNodeMap(deeperOpenMoves, depth + 1,tempNodeMap)
          negaMax(tempBoard, deeperNodeMap, depth + 1, maxToken, minToken, changeToken, depthLimit)
        }
      }
    }
  }
}
