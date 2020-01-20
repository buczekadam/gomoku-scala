package board

case class Cell(x: Int, y: Int) extends Ordered[Cell] {
  override def compare(that: Cell): Int =
    if (this.y == that.y) {
      this.x.compareTo(that.x)
    } else {
      this.y.compareTo(that.y)
    }
}
