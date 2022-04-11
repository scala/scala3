package i5854

class B {
  // Known issue: Can't lookup the symbol of `b.A`
  // we have to register the symbol of `b { type A }` to the refinementSymtab first
  // then resolve, or assign same semanticdb symbol for both
  // fake symbol for b.A, and real symbol of A in b
  val a: String = (((1: Any): b.A): Nothing): String
  val b: { type A >: Any <: Nothing } = loop()         // error
  def loop(): Nothing = loop()
}
