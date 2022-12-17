package dotty.util
import scala.util.control.ControlThrowable

object boundary:

  class Break[T](val label: Label[T], val value: T) extends ControlThrowable

  class Label[T] extends ControlThrowable:
    transparent inline def break(value: T): Nothing = throw Break(this, value)

  transparent inline def apply[T <: R, R](inline body: Label[T] ?=> R): R =
    val local = Label[T]()
    try body(using local)
    catch case ex: Break[_] if ex.label eq local =>
      ex.value.asInstanceOf[T]

end boundary

object break:
  transparent inline def apply[T](value: T)(using l: boundary.Label[T]): Nothing =
    l.break(value)
