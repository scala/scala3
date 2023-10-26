class Ifce[BT <: Boolean] extends Selectable:
  type RT = BT match
    case true  => this.type { val v1: Int }
    case false => this.type
  def cast : RT = this.asInstanceOf[RT]
  def selectDynamic(key: String): Any = ???

class Test:
  def t1: Unit =
    val full = (new Ifce[true]).cast
    val v1 = full.v1
