package scala.tasty.types

import scala.tasty.Extractor

trait RecursiveType extends Type

object RecursiveType {
  type Data = Type
  def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyRecursiveType(arg)
}
