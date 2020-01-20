import board.{Board, Cell}
import game.Player.Player
import game.{Game, Player}

import scala.io.StdIn

object Gomoku extends App {
  var board = new Board()
  val game = new Game
  var currentPlayer: Player = Player.Black
  gameLoop


  def gameLoop: Unit = {
    board.printBoard
    while (true) {
      userMove
      if (game.isPlayerWon(board, currentPlayer)) {
        board.printBoard
        print("Wygrywa Gracz!")
        return
      }
      board.printBoard
      currentPlayer = game.switchPlayer(currentPlayer)

      board = game.minMaxMove(board, currentPlayer)
      if (game.isPlayerWon(board, currentPlayer)) {
        board.printBoard
        print("Wygrywa MinMax!")
        return
      }
      board.printBoard
      currentPlayer = game.switchPlayer(currentPlayer)
    }
  }

  def userMove: Unit = {
    print("Podaj współrzędne: ")
    val input = StdIn.readLine()
    val cell = getCoordinatesFromInput(input)
    if (!board.isCellCorrect(cell)) {
      println("Błędne współrzędne, podaj właściwe")
      userMove
      return
    }
    if (!board.isCellEmpty(cell)) {
      println("Pole jest już zajęte, podaj inne współrzędne")
      userMove
      return
    }
    board.addCellToBoard(cell, currentPlayer)
  }

  def getCoordinatesFromInput(stdInput: String): Cell = {
    val y = stdInput.head - 'a'
    val x = stdInput.tail.toInt
    Cell(x, y)
  }
}
