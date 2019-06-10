package scala

package quoted {
  import scala.internal.quoted.TaggedType

  sealed abstract class Type[T <: AnyKind] {
    type `$splice` = T
  }

  /** Some basic type tags, currently incomplete */
  object Type {

    implicit object TypeOps {
      /** Show a source code like representation of this type */
      def (tpe: Type[T]) show[T] given Toolbox: String = the[Toolbox].show(tpe.asInstanceOf[Type[Any]])
    }

    implicit val UnitTag: Type[Unit] = new TaggedType[Unit]
    implicit val BooleanTag: Type[Boolean] = new TaggedType[Boolean]
    implicit val ByteTag: Type[Byte] = new TaggedType[Byte]
    implicit val CharTag: Type[Char] = new TaggedType[Char]
    implicit val ShortTag: Type[Short] = new TaggedType[Short]
    implicit val IntTag: Type[Int] = new TaggedType[Int]
    implicit val LongTag: Type[Long] = new TaggedType[Long]
    implicit val FloatTag: Type[Float] = new TaggedType[Float]
    implicit val DoubleTag: Type[Double] = new TaggedType[Double]
  }

}

package internal {
  package quoted {
    import scala.quoted.Type
    import scala.reflect.ClassTag
    import scala.runtime.quoted.Unpickler.Pickled

    /** A Type backed by a pickled TASTY tree */
    final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends Type[T] {
      override def toString(): String = s"Type(<pickled tasty>)"
    }

    /** An Type backed by a value */
    final class TaggedType[T](implicit val ct: ClassTag[T]) extends Type[T] {
      override def toString: String = s"Type($ct)"
    }

    /** An Type backed by a tree */
    final class TreeType[Tree](val typeTree: Tree) extends Type[Any] {
      override def toString: String = s"Type(<tasty tree>)"
    }

  }
}
