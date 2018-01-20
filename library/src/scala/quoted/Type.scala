package scala.quoted

import scala.reflect.ClassTag

abstract class Type[T] extends Quoted {
  type unary_~ = T
}

/** Some basic type tags, currently incomplete */
object Type {

  class TaggedPrimitive[T] private[Type] (implicit val ct: ClassTag[T]) extends Type[T] {
    override def toString: String = s"Type($ct)"
  }

  implicit def UnitTag: Type[Unit] = new TaggedPrimitive[Unit]
  implicit def BooleanTag: Type[Boolean] = new TaggedPrimitive[Boolean]
  implicit def ByteTag: Type[Byte] = new TaggedPrimitive[Byte]
  implicit def CharTag: Type[Char] = new TaggedPrimitive[Char]
  implicit def ShortTag: Type[Short] = new TaggedPrimitive[Short]
  implicit def IntTag: Type[Int] = new TaggedPrimitive[Int]
  implicit def LongTag: Type[Long] = new TaggedPrimitive[Long]
  implicit def FloatTag: Type[Float] = new TaggedPrimitive[Float]
  implicit def DoubleTag: Type[Double] = new TaggedPrimitive[Double]
}
