package dotty

import scala.language.higherKinds
import scala.collection.GenTraversableOnce

/**
 * @author Sébastien Doeraene
 */
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
    //  @inline // only for Scala.js?
    def apply[A](value: A): USome[A] = value match {
      case value: UNone.type  => value.wrap.asInstanceOf[USome[A]]
      case value: WrappedNone => value.wrap.asInstanceOf[USome[A]]
      case _                  => value.asInstanceOf[USome[A]]
    }

    // UOptionOps fits the contract of name-based pattern matching
    def unapply[A](option: UOption[A]): UOptionOps[A] =
      new UOptionOps(option)
  }

  implicit class UOptionOps[A](private val self: UOption[A]) extends AnyVal {
    @inline def isEmpty: Boolean = self eq UNone
    @inline def nonEmpty: Boolean = !isEmpty
    @inline def isDefined: Boolean = !isEmpty

    /** Must not be called when `isEmpty` is `true`! */
    // @inline // only for Scala.js?
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

    @inline def flatMap[B](f: A => UOption[B]): UOption[B] =
      if (isEmpty) UNone else f(forceGet)

    def flatten[B](implicit ev: A <:< UOption[B]): UOption[B] =
      if (isEmpty) UNone else ev(forceGet)

    @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
      if (isEmpty) ifEmpty else f(forceGet)

    @inline def filter(p: A => Boolean): UOption[A] =
      if (isEmpty || p(forceGet)) self else UNone

    @inline def withFilter(p: A => Boolean): WithFilter[A] = new WithFilter[A](self, p)

    @inline def find(p: A => Boolean): UOption[A] = filter(p)

    @inline def exists(p: A => Boolean): Boolean =
      !isEmpty && p(forceGet)

    @inline def forall(p: A => Boolean): Boolean =
      isEmpty || p(forceGet)

    @inline def getOrElse[B >: A](ifEmpty: => B): B =
      if (isEmpty) ifEmpty else forceGet

    @inline final def orElse[B >: A](alternative: => UOption[B]): UOption[B] =
      if (isEmpty) alternative else self.asInstanceOf[UOption[B]]

    @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = getOrElse(ev(null))

    @inline def foreach[U](f: A => U): Unit = {
      if (isDefined) f(forceGet)
    }

    @inline def toSeq: Seq[A] = iterator.toSeq
    @inline def toList: List[A] = iterator.toList

    @inline def iterator: Iterator[A] =
      if (isEmpty) Iterator.empty
      else Iterator.single(forceGet)

    @deprecated("", "")
    def toOption: Option[A] =
      if (isEmpty) None
      else Some(forceGet)
  }

  implicit class OptionOps[A](private val self: Option[A]) extends AnyVal {
    @deprecated("", "")
    def toUOption: UOption[A] =
      if (self.isEmpty) UNone
      else USome(self.get)
  }

  implicit class SomeOps[A](private val self: Some[A]) extends AnyVal {
    @deprecated("", "")
    def toUSome: USome[A] = USome(self.get)
  }

  implicit class PartialFunctionOps[A, B](private val pf: PartialFunction[A, B]) extends AnyVal {
    def uLift: A => UOption[B] = new Lifted[A, B](pf)
  }

  class WithFilter[A](self: UOption[A], p: A => Boolean) {
    def map[B](f: A => B): UOption[B] = self filter p map f
    def flatMap[B](f: A => UOption[B]): UOption[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](self, x => p(x) && q(x))
  }

  private class Lifted[-A, +B] (val pf: PartialFunction[A, B])
    extends scala.runtime.AbstractFunction1[A, UOption[B]] with Serializable {

    def apply(x: A): UOption[B] = {
      if (pf.isDefinedAt(x)) USome(pf.apply(x)) else UNone
    }
  }
}
