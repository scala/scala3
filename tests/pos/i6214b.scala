object Test {
  def res(x: quoted.Expr[Int]) given tasty.Reflection: quoted.Expr[Int] = x match {
    case '{ val a: Int = ${ Foo('{ val b: Int = $y; b }) }; a } => y // owner of y is res
  }
  object Foo {
    def unapply(x: quoted.Expr[Int]): Option[quoted.Expr[Int]] = Some(x)
  }
}
