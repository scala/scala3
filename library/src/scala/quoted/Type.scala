package scala.quoted

import scala.quoted.Quoted.TaggedType

abstract class Type[T] extends Quoted {
  type unary_~ = T
}

/** Some basic type tags, currently incomplete */
object Type {
  implicit def UnitTag: Type[Unit] = new TaggedType[Unit]
  implicit def BooleanTag: Type[Boolean] = new TaggedType[Boolean]
  implicit def ByteTag: Type[Byte] = new TaggedType[Byte]
  implicit def CharTag: Type[Char] = new TaggedType[Char]
  implicit def ShortTag: Type[Short] = new TaggedType[Short]
  implicit def IntTag: Type[Int] = new TaggedType[Int]
  implicit def LongTag: Type[Long] = new TaggedType[Long]
  implicit def FloatTag: Type[Float] = new TaggedType[Float]
  implicit def DoubleTag: Type[Double] = new TaggedType[Double]
}
