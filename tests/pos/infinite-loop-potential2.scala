trait Box[Elem]
trait Chain extends Box[Chain]
trait Fresh[+C <: Chain & Box[C]]

class Test:
  def meth(any: Any): Unit = any match
    case _: Fresh[c] =>
