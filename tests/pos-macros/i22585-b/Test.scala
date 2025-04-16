trait Hammer[I, O] {
  def hammer(input: I): O
}

object HammerSpec {
  case class A(x: Int)
  case class B(x: Int)
  Hammer2.makeProductHammerMacro[A, B]()
}