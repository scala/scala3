//> using options  -Wimplausible-patterns
sealed trait Exp[T]
case class IntExp(x: Int) extends Exp[Int]
case class StrExp(x: String) extends Exp[String]
object UnitExp extends Exp[Unit]

trait Txn[T <: Txn[T]]
case class Obj(o: AnyRef) extends Txn[Obj] with Exp[AnyRef]


class Foo {
  def bar[A <: Txn[A]](x: Exp[A]): Unit = x match
    case IntExp(x) =>
    case StrExp(x) =>
    case UnitExp => // warn
    case Obj(o) =>
}

