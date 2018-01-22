package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

/** A Type backed by a pickled TASTY tree */
final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends Type[T] with TastyQuoted {
  override def toString(): String = s"Type(<pickled>)"
}
