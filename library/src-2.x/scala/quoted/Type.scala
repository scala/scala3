package scala

package quoted {
  import scala.internal.quoted.TaggedType

  sealed abstract class Type[T] {
    type `$splice` = T
  }

  /** Some basic type tags, currently incomplete */
  object Type {

    implicit class TypeOps[T](tpe: Type[T]) {
      /** Show a source code like representation of this type */
      def show(implicit toolbox: Toolbox): String = toolbox.show(tpe.asInstanceOf[Type[Any]])
    }

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

}

package internal {
  package quoted {
    import scala.quote.Type
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
    final class TreeType[Tree](val typeTree: Tree) extends quoted.Type[Any] {
      override def toString: String = s"Type(<tasty tree>)"
    }

  }
}
