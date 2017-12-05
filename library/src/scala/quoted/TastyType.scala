package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

/** A Type backed by a pickled TASTY tree */
final case class TastyType[T](tasty: Pickled, args: Seq[Any]) extends Type[T] with TastyQuoted
