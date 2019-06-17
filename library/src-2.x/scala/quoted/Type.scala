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

    implicit def UnitTag: Type[Unit] = new scala.internal.quoted.TaggedType[Unit]
    implicit def BooleanTag: Type[Boolean] = new scala.internal.quoted.TaggedType[Boolean]
    implicit def ByteTag: Type[Byte] = new scala.internal.quoted.TaggedType[Byte]
    implicit def CharTag: Type[Char] = new scala.internal.quoted.TaggedType[Char]
    implicit def ShortTag: Type[Short] = new scala.internal.quoted.TaggedType[Short]
    implicit def IntTag: Type[Int] = new scala.internal.quoted.TaggedType[Int]
    implicit def LongTag: Type[Long] = new scala.internal.quoted.TaggedType[Long]
    implicit def FloatTag: Type[Float] = new scala.internal.quoted.TaggedType[Float]
    implicit def DoubleTag: Type[Double] = new scala.internal.quoted.TaggedType[Double]
  }

}

package internal {
  package quoted {
    import scala.reflect.ClassTag
    import scala.runtime.quoted.Unpickler.Pickled

    /** A Type backed by a pickled TASTY tree */
    final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends scala.quoted.Type[T] {
      override def toString(): String = s"Type(<pickled tasty>)"
    }

    /** An Type backed by a value */
    final class TaggedType[T](implicit val ct: ClassTag[T]) extends scala.quoted.Type[T] {
      override def toString: String = s"Type($ct)"
    }

    /** An Type backed by a tree */
    final class TreeType[Tree](val typeTree: Tree) extends scala.quoted.Type[Any] {
      override def toString: String = s"Type(<tasty tree>)"
    }

  }
}
