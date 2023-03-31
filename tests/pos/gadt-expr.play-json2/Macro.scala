final case class JsNumber()

sealed trait Val[+A]
final case class Box[T](value: T) extends Val[T]

trait Reads[A]:
  def reads(json: JsNumber): Val[A]
object Reads:
  given Reads[Int] = { case JsNumber() => Box(0) case null => ??? }

object Macro:
  import scala.compiletime.*, scala.deriving.*

  inline def reads[A](using m: Mirror.ProductOf[A]): Reads[A] = new Reads[A]:
    def reads(js: JsNumber) = rec[A, m.MirroredElemLabels, m.MirroredElemTypes](js)

  inline def rec[A, L <: Tuple, T <: Tuple](js: JsNumber)(using m: Mirror.ProductOf[A]): Val[A] =
    inline (erasedValue[L], erasedValue[T]) match
      case _: (EmptyTuple, EmptyTuple) => ???
      case _: (   l *: ls,    t *: ts) => summonInline[Reads[t]].reads(js) match
        case Box(x) => rec[A, ls, ts](js)

final case class Foo(bar: Int, baz: Int)
