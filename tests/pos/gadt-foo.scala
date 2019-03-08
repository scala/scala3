object foo {
  sealed trait Exp[T]
  case class Var[T](name: String) extends Exp[T]

  def env[T](x: Var[T]): T = ???

  def eval[S](e: Exp[S]) = e match {
    case v: Var[foo] =>
      env(v)
  }
}
