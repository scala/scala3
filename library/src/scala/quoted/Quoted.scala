package scala.quoted

import scala.reflect.ClassTag
import scala.runtime.quoted.Unpickler.Pickled

/** Common superclass of Expr and Type */
abstract class Quoted

object Quoted {

  /** A quote backed by a pickled TASTY tree */
  trait TastyQuoted extends Quoted {
    def tasty: Pickled
    def args: Seq[Any]
  }

  /** Quoted for which its internal representation is its tree.
   *  - Used for trees that cannot be serialized, such as references to local symbols that will be spliced in.
   *  - Used for trees that do not need to be serialized to avoid the overhead of serialization/deserialization.
   */
  trait RawQuoted[Tree] extends quoted.Quoted {
    def tree: Tree
  }

  // Implementations of Expr[T]

  /** An Expr backed by a pickled TASTY tree */
  final class TastyExpr[T](val tasty: Pickled, val args: Seq[Any]) extends Expr[T] with TastyQuoted {
    override def toString(): String = s"Expr(<pickled>)"
  }

  /** An Expr backed by a value */
  final class ConstantExpr[T](val value: T) extends Expr[T] {
    override def toString: String = s"Expr($value)"
  }

  /** An Expr backed by a tree */
  final class RawExpr[Tree](val tree: Tree) extends quoted.Expr[Any] with RawQuoted[Tree]

  /** An Expr representing `'{(~f).apply(~x)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[T, U](val f: Expr[T => U], val x: Expr[T]) extends Expr[U] {
    override def toString: String = s"Expr($f <applied to> $x)"
  }

  // Implementations of Type[T]

  /** A Type backed by a pickled TASTY tree */
  final class TastyType[T](val tasty: Pickled, val args: Seq[Any]) extends Type[T] with TastyQuoted {
    override def toString(): String = s"Type(<pickled>)"
  }

  /** An Type backed by a value */
  final class TaggedType[T](implicit val ct: ClassTag[T]) extends Type[T] {
    override def toString: String = s"Type($ct)"
  }

  /** An Type backed by a tree */
  final class RawType[Tree](val tree: Tree) extends quoted.Type[Any] with RawQuoted[Tree]

}
