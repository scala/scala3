package scala.util
import boundary.{break, Label}

/** A mockup of scala.Option */
abstract class Option[+T]
case class Some[+T](x: T) extends Option[T]
case object None extends Option[Nothing]

object Option:
  /** This extension should be added to the companion object of scala.Option */
  extension [T](r: Option[T])
    inline def ? (using label: Label[None.type]): T = r match
      case Some(x) => x
      case None => break(None)

/** A prompt for `Option`, which establishes a boundary which `_.?` on `Option` can return */
object optional:
  inline def apply[T](inline body: Label[None.type] ?=> T): Option[T] =
    boundary(Some(body))

