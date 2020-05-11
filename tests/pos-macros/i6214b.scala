import scala.quoted.Scope
object Test {
  def res(using s: Scope)(x: s.Expr[Int]): s.Expr[Int] = x match {
    case '{ val a: Int = ${ Foo('{ val b: Int = $y; b }) }; a } => y // owner of y is res
  }
  object Foo {
    def unapply(using s: Scope)(x: s.Expr[Int]): Option[s.Expr[Int]] = Some(x)
  }
}
