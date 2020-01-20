package game

object Player extends Enumeration {
  type Player = Value
  val Black: Player.Value = Value("X")
  val White: Player.Value = Value("O")
}
