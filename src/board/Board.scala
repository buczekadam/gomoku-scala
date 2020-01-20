package board

import game.Player.Player

import scala.collection.mutable

class Board(private val size: Int = 19, private val board: mutable.HashMap[Cell, Player] = mutable.HashMap.empty) {

  def isCellEmpty(cell: Cell): Boolean =
    !board.contains(cell)

  def isCellCorrect(cell: Cell): Boolean =
    cell.x >= 0 && cell.x < size && cell.y >= 0 && cell.y < size

  def getPlayerCells(player: Player): List[Cell] = {
    board.filter(_._2 == player).keys.toList.sorted
  }

  def getEmptyCells(): List[Cell] = {
    var resultList: List[Cell] = List()
    (0 until size).foreach(row => {
      (0 until size).foreach(column => {
        val cell: Cell = Cell(row, column)
        if (!board.contains(cell)) resultList = resultList ++ List(cell)
      })
    })
    resultList
  }

  def addCellToBoard(cell: Cell, player: Player) =
    board += cell -> player

  def newBoard(cell: Cell, player: Player) =
    new Board(size, board + (cell -> player))

  def printBoard = {
    printHeadRow
    (0 until size).foreach(rowNumber => {
      printRow(rowNumber)
    })
    println()
  }

  def printHeadRow =
    print("   | " + ('A' to 'S').mkString(" | ") + " |\n")

  def printRow(rowNumber: Int) = {
    if (rowNumber < 10) {
      print(" " + rowNumber + " |")
    } else {
      print(rowNumber + " |")
    }
    (0 until size).foreach(columnNumber => {
      printCell(rowNumber, columnNumber)
    })
    println()
  }

  def printCell(row: Int, column: Int) = {
    val cell = Cell(row, column)
    print(" " + board.get(cell).map(_.toString).getOrElse("-") + " |")
  }
}
