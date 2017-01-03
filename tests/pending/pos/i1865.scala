class AbsCell {
  type T = Node
  class Node
}

object Test {
  def test: Unit = {
    val cell = new AbsCell
    new cell.T
  }
}

class AbsCell2 {
  type T = Node
  val value: T = value
  def set(x: T): Unit = {}
  class Node
}
object init {
  def main = {
    val cell = new AbsCell2 { val init = new Node }
    cell set (new cell.T)
  }
}
