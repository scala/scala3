package scala.quoted

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
trait Liftable[T] {

  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T) given QuoteContext: Expr[T]

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
    def toExpr(x: T) given (qctx: QuoteContext): Expr[T] = {
      import qctx.tasty._
      Literal(Constant(x)).seal.asInstanceOf[Expr[T]]
    }
  }

  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new Liftable[Class[T]] {
    /** Lift a `Class[T]` into `'{ classOf[T] }` */
    def toExpr(x: Class[T]) given (qctx: QuoteContext): Expr[Class[T]] = {
      import qctx.tasty._
      Ref(definitions.Predef_classOf).appliedToType(Type(x)).seal.asInstanceOf[Expr[Class[T]]]
    }
  }

}
