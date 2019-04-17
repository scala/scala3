//import scala.quoted.matching.Bind
object Test {
  def res(x: quoted.Expr[Int]) given tasty.Reflection: quoted.Expr[Int] = x match {
    case '{ 1 + (${Bind(b)}: Int) } => ??? // error: Not found: Bind
    case _ => ???
  }
}
