import language.experimental.captureChecking
import caps.*

object boundary:
  class Label[-T]
  case class Break[T](private[boundary] label: Label[T]^{}, result: T) extends Throwable

  def apply[T](body: Label[T]^ ?=> T): T =
    val label = Label()
    try
      body(using label)
    catch
      case Break[T @unchecked](l, res) if l eq label => res

  def break[T](value: T)(using l: Label[T]) = throw Break(unsafe.unsafeAssumePure(l), value)

def test =
  boundary[boundary.Label[Int]^]: l1 ?=>
    boundary[Int]: l2 ?=>
      boundary.break(l2)(using l1) // error
      15
    ???