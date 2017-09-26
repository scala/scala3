import scala.language.higherKinds

package object uoption {
  private[uoption] final class WrappedNone(
    depth: Int,
    val unwrap: Any // UNone or WrappedNone
  ) {
    lazy val wrap: WrappedNone =
      new WrappedNone(depth + 1, this)

    private val stringRepr: String =
      ("USome(" * depth) + "UNone" + (")" * depth)

    override def toString(): String = stringRepr
  }

  object UNone {
    private[uoption] val wrap: WrappedNone = new WrappedNone(1, this)

    override def toString(): String = "UNone"
  }

  type UOption[+A] >: UNone.type <: AnyRef

  object UOption {
    @inline // only for Scala.js?
    def apply[A](x: A): UOption[A] =
      if (x == null) UNone
      else USome(x)
  }

  type USome[+A] <: UOption[A]

  object USome {
    @inline // only for Scala.js?
    def apply[A](value: A): USome[A] = value match {
      case value @ UNone      => value.wrap.asInstanceOf[USome[A]]
      case value: WrappedNone => value.wrap.asInstanceOf[USome[A]]
      case _                  => value.asInstanceOf[USome[A]]
    }

    // UOptionOps fits the contract of name-based pattern matching
    def unapply[A](option: UOption[A]): UOptionOps[A] =
      new UOptionOps(option)
  }

  implicit class UOptionOps[A](private val self: UOption[A]) extends AnyVal {
    @inline def isEmpty: Boolean = self eq UNone
    @inline def isDefined: Boolean = !isEmpty

    /** Must not be called when `isEmpty` is `true`! */
    @inline // only for Scala.js?
    private def forceGet: A = (self: Any) match {
      case none: WrappedNone =>
        none.unwrap.asInstanceOf[A]
      case _ =>
        self.asInstanceOf[A]
    }

    @inline // is this a good idea at all?
    def get: A =
      if (isEmpty) throw new NoSuchElementException("UNone.get")
      else forceGet

    @inline def map[B](f: A => B): UOption[B] =
      if (isEmpty) self.asInstanceOf[UOption[B]]
      else USome(f(forceGet))

    @inline def getOrElse[B >: A](ifEmpty: => B): B =
      if (isEmpty) ifEmpty
      else forceGet
  }
}
