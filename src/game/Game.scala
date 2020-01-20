package game

import board.{Board, Cell}
import game.Player.Player

import scala.annotation.tailrec

class Game {

  def isPlayerWon(board: Board, player: Player): Boolean = {
    val playerCells = board.getPlayerCells(player)
    (-1 to 1).foreach(x => {
      (-1 to 1).foreach(y => {
        playerCells.foreach(playerCell => {
          if ((x != 0 || y != 0) && isFiveInDirection(1, playerCell, playerCells, (x, y))) {
            return true
          }
        })
      })
    })
    false
  }

  @tailrec
  private def isFiveInDirection(counter: Int, cell: Cell, coordinatesList: List[Cell], direction: (Int, Int)): Boolean =
    if (coordinatesList.contains(cell) && counter == 5) {
      true
    } else if (coordinatesList.contains(cell)) {
      isFiveInDirection(counter + 1, Cell(cell.x + direction._1, cell.y + direction._2), coordinatesList, direction)
    } else {
      false
    }

  def switchPlayer(player: Player): Player =
    if (player == Player.White) {
      Player.Black
    } else {
      Player.White
    }


  def rateBoard(board: Board, player: Player): Int = {
    ratePlayerCells(board.getPlayerCells(player)) - ratePlayerCells(board.getPlayerCells(switchPlayer(player)))
  }

  @tailrec
  private def rateCellsInDirection(rateSum: Int, vectorLength: Long, cell: Cell, cells: List[Cell], direction: (Int, Int)): Int = {
    if (cells.isEmpty) {
      rateSum + Math.pow(16, vectorLength).toInt
    } else if (cells.contains(cell)) {
      rateCellsInDirection(rateSum, vectorLength + 1, Cell(cell.x + direction._1, cell.y + direction._2), cells diff List(cell), direction)
    } else {
      rateCellsInDirection(rateSum + Math.pow(5, vectorLength).toInt, 0, Cell(cells.head.x + direction._1, cells.head.y + direction._2), cells.tail, direction)
    }
  }

  private def ratePlayerCells(cells: List[Cell]): Int = {
    var resultSum: Int = 0
    (-1 to 1).foreach(x => {
      (-1 to 1).foreach(y => {
        if ((x != 0 || y != 0) && cells.nonEmpty) {
          resultSum += rateCellsInDirection(0, 0, cells.head, cells, (x, y))
        }
      })
    })
    resultSum
  }

  private def createTree(player: Player, board: Board, possibleMoves: List[Cell]): Tree[Board] = {
    var nodes: LazyList[Tree[Board]] = LazyList()
    nodes = possibleMoves.to(LazyList).map(possibleMove => {
      var newPossibleMoves: List[Cell] = possibleMoves diff List(possibleMove)
      (-1 to 1).foreach(x => {
        (-1 to 1).foreach(y => {
          if (x != 0 || y != 0) {
            val neighbour: Cell = Cell(possibleMove.x + x, possibleMove.y + y)
            if (board.isCellCorrect(neighbour) && board.isCellEmpty(neighbour)) {
              newPossibleMoves = (newPossibleMoves ++ List(neighbour)).distinct
            }
          }
        })
      })
      createTree(switchPlayer(player), board.newBoard(possibleMove, player), newPossibleMoves)
    })
    Tree(board, nodes)
  }

  private def getPossibleMoves(board: Board): List[Cell] = {
    val result: List[Cell] = board.getEmptyCells().filter(cell => {
      var resultBool = false
      (-1 to 1).foreach(x => {
        (-1 to 1).foreach(y => {
          if (x != 0 || y != 0) {
            if (!board.isCellEmpty(Cell(cell.x + x, cell.y + y))) {
              resultBool = true
            }
          }
        })
      })
      resultBool
    })
    result
  }

  def minMaxMove(board: Board, player: Player): Board = {
    val possibleMoves: List[Cell] = getPossibleMoves(board)
    if (possibleMoves.isEmpty) {
      board.addCellToBoard(board.getEmptyCells().head, player)
    }
    val tree: Tree[Board] = createTree(player, board, possibleMoves)
    tree.nodes(getMaxBoard(3, player, tree)._1).data
  }

  private def getMaxBoard(depth: Int, player: Player, boardTree: Tree[Board]): (Int, Int) = {
    if (depth == 0) {
      val maxList: LazyList[Int] = boardTree.nodes.map(node => rateBoard(node.data, player))
      return (maxList.indexOf(maxList.max), maxList.max)
    }
    val maxList: LazyList[Int] = boardTree.nodes.map(node => {
      getMinBoard(depth - 1, player, node)._2
    })
    (maxList.indexOf(maxList.max), maxList.max)
  }

  private def getMinBoard(depth: Int, player: Player, boardTree: Tree[Board]): (Int, Int) = {
    if (depth == 0) {
      val minList: LazyList[Int] = boardTree.nodes.map(node => rateBoard(node.data, player))
      return (minList.indexOf(minList.min), minList.min)
    }
    val minList: LazyList[Int] = boardTree.nodes.map(node => {
      getMaxBoard(depth - 1, player, node)._2
    })
    (minList.indexOf(minList.min), minList.min)
  }
}
