class Ifce[BT <: Boolean]:
  type RT = BT match
    case true  => this.type { val v1: Int }
    case false => this.type
  def cast: RT = this.asInstanceOf[RT]

class Test:
  def t1: Unit =
    val full1 = new Ifce[true]().cast
    val v1 = full1.v1 // error
//           ^^^^^
//           Found:    (full1 : Ifce[(true : Boolean)]#RT)
//           Required: Selectable | Dynamic
