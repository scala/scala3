package scala.quoted

import scala.quoted.Types.TaggedType
import scala.reflect.ClassTag
import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Type[T <: AnyKind] {
  type `$splice` = T
}

/** Some basic type tags, currently incomplete */
object Type {

  implicit object TypeOps {
    /** Show a source code like representation of this type */
    def (tpe: Type[T]) show[T] given Toolbox: String = the[Toolbox].show(tpe.asInstanceOf[Type[Any]])
  }

  implied UnitTag for Type[Unit] = new TaggedType[Unit]
  implied BooleanTag for Type[Boolean] = new TaggedType[Boolean]
  implied ByteTag for Type[Byte] = new TaggedType[Byte]
  implied CharTag for Type[Char] = new TaggedType[Char]
  implied ShortTag for Type[Short] = new TaggedType[Short]
  implied IntTag for Type[Int] = new TaggedType[Int]
  implied LongTag for Type[Long] = new TaggedType[Long]
  implied FloatTag for Type[Float] = new TaggedType[Float]
  implied DoubleTag for Type[Double] = new TaggedType[Double]
}

/** All implementations of Type[T].
 *  These should never be used directly.
 */
object Types {
  /** A Type backed by a pickled TASTY tree */
  final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends Type[T] {
    override def toString(): String = s"Type(<pickled tasty>)"
  }

  /** An Type backed by a value */
  final class TaggedType[T](implicit val ct: ClassTag[T]) extends Type[T] {
    override def toString: String = s"Type($ct)"
  }

  /** An Type backed by a tree */
  final class TreeType[Tree](val typeTree: Tree) extends quoted.Type[Any] {
    override def toString: String = s"Type(<tasty tree>)"
  }
}
