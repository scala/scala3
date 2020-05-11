object Test {
  def res(using s: quoted.Scope)(x: s.Expr[Int]): s.Expr[Int] = x match {
    case '{ 1 + (${Bind(b)}: Int) } => ??? // error: Not found: Bind
    case _ => ???
  }
}
