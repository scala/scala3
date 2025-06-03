//> using options  -Wimplausible-patterns

sealed trait Exp[T]
case class IntExp(x: Int) extends Exp[Int]
case class StrExp(x: String) extends Exp[String]
object UnitExp extends Exp[Unit]

class Foo[U <: Int, T <: U] {
  def bar[A <: T](x: Exp[A]): Unit = x match
    case IntExp(x) =>
    case StrExp(x) => // warn
    case UnitExp => // warn implausible pattern
}
