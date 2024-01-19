trait Elems {
  sealed class Elem[A] extends Dummy

  val UnitElement: Elem[Unit]

  trait Dummy
}

class BadMatch[A <: Elems](a: A) {
  private def toLuaValue(eX: a.Elem[?]): String = eX match {
    case a.UnitElement => "" // type mismatch
  }
}