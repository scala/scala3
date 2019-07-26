package scala.quoted

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

  given Liftable_Boolean_delegate as Liftable[Boolean] = new PrimitiveLiftable
  given Liftable_Byte_delegate as Liftable[Byte] = new PrimitiveLiftable
  given Liftable_Short_delegate as Liftable[Short] = new PrimitiveLiftable
  given Liftable_Int_delegate as Liftable[Int] = new PrimitiveLiftable
  given Liftable_Long_delegate as Liftable[Long] = new PrimitiveLiftable
  given Liftable_Float_delegate as Liftable[Float] = new PrimitiveLiftable
  given Liftable_Double_delegate as Liftable[Double] = new PrimitiveLiftable
  given Liftable_Char_delegate as Liftable[Char] = new PrimitiveLiftable
  given Liftable_String_delegate as Liftable[String] = new PrimitiveLiftable

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

}
