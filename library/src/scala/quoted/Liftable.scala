package scala.quoted

import scala.reflect.ClassTag

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
trait Liftable[T] {

  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): given QuoteContext => Expr[T]

}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  delegate Liftable_Boolean_delegate for Liftable[Boolean] = new PrimitiveLiftable
  delegate Liftable_Byte_delegate for Liftable[Byte] = new PrimitiveLiftable
  delegate Liftable_Short_delegate for Liftable[Short] = new PrimitiveLiftable
  delegate Liftable_Int_delegate for Liftable[Int] = new PrimitiveLiftable
  delegate Liftable_Long_delegate for Liftable[Long] = new PrimitiveLiftable
  delegate Liftable_Float_delegate for Liftable[Float] = new PrimitiveLiftable
  delegate Liftable_Double_delegate for Liftable[Double] = new PrimitiveLiftable
  delegate Liftable_Char_delegate for Liftable[Char] = new PrimitiveLiftable
  delegate Liftable_String_delegate for Liftable[String] = new PrimitiveLiftable

  private class PrimitiveLiftable[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Liftable[T] {
    /** Lift a primitive value `n` into `'{ n }` */
    def toExpr(x: T) = given qctx => {
      import qctx.tasty._
      Literal(Constant(x)).seal.asInstanceOf[Expr[T]]
    }
  }

  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new Liftable[Class[T]] {
    /** Lift a `Class[T]` into `'{ classOf[T] }` */
    def toExpr(x: Class[T]) = given qctx => {
      import qctx.tasty._
      Ref(definitions.Predef_classOf).appliedToType(Type(x)).seal.asInstanceOf[Expr[Class[T]]]
    }
  }

  given [T: Type: Liftable: ClassTag] as Liftable[IArray[T]] = new Liftable[IArray[T]] {
    def toExpr(iarray: IArray[T]): given QuoteContext => Expr[IArray[T]] = '{
      val array = new Array[T](${iarray.length.toExpr})(ClassTag(${the[ClassTag[T]].runtimeClass.toExpr}))
      ${ Expr.block(List.tabulate(iarray.length)(i => '{ array(${i.toExpr}) = ${iarray(i).toExpr} }), '{ array.asInstanceOf[IArray[T]] }) }
    }
  }

  given [T: Type: Liftable] as Liftable[Seq[T]] = new Liftable[Seq[T]] {
    def toExpr(seq: Seq[T]): given QuoteContext => Expr[Seq[T]] = given qctx => {
      import qctx.tasty._
      Repeated(seq.map(x => the[Liftable[T]].toExpr(x).unseal).toList, the[quoted.Type[T]].unseal).seal.asInstanceOf[Expr[Seq[T]]]
    }
  }

  given [T: Type: Liftable] as Liftable[List[T]] = new Liftable[List[T]] {
    def toExpr(x: List[T]): given QuoteContext => Expr[List[T]] = x match {
      case x :: xs  => '{ (${xs.toExpr}).::[T](${x.toExpr}) }
      case Nil => '{ Nil: List[T] }
    }
  }

  given [T: Type: Liftable] as Liftable[Option[T]] = new Liftable[Option[T]] {
    def toExpr(x: Option[T]): given QuoteContext => Expr[Option[T]] = x match {
      case Some(x) => '{ Some[T](${x.toExpr}) }
      case None => '{ None: Option[T] }
    }
  }

  given [L: Type: Liftable, R: Type: Liftable] as Liftable[Either[L, R]] = new Liftable[Either[L, R]] {
    def toExpr(x: Either[L, R]): given QuoteContext => Expr[Either[L, R]] = x match {
      case Left(x) => '{ Left[L, R](${x.toExpr}) }
      case Right(x) => '{ Right[L, R](${x.toExpr}) }
    }
  }

}
