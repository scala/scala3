class Type
class Symbol

sealed trait Space
case object Empty extends Space
case class Typ(tp: Type, decomposed: Boolean) extends Space
case class Prod(tp: Type, unappTp: Type, unappSym: Symbol, params: List[Space], full: Boolean) extends Space
case class Or(spaces: List[Space]) extends Space

object Test {
  def simplify(space: Space, aggressive: Boolean = false): Space = space match {
    case Prod(tp, fun, sym, spaces, full) =>
      val sp = Prod(tp, fun, sym, spaces.map(simplify(_)), full)
      if (sp.params.contains(Empty)) Empty
      else sp
    case Or(spaces) => space
    case Typ(tp, _) => space
    case _ => space
  }

  def main(args: Array[String]): Unit = {
    simplify(Prod(new Type, new Type, new Symbol, Nil, false))
  }
}
