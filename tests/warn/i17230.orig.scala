//> using options -Werror
import scala.util.*

trait Transaction {
  type State[_]
}
object Transaction {
  type of[S[_]] = Transaction { type State[A] = S[A] }
}
trait DynamicScope[State[_]]

case class ScopeSearch[State[_]](self: Either[Transaction.of[State], DynamicScope[State]]) {

  def embedTransaction[T](f: Transaction.of[State] => T): T =
    self match {
      case Left(integrated) => ???
      case Right(ds)        => ???
    }
}

