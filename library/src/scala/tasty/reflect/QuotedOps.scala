package scala.tasty.reflect

/** Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects */
trait QuotedOps extends Core {

  trait QuotedExprAPI {
    /** View this expression `Expr[T]` as a `Term` */
    def unseal(implicit ctx: Context): Term
  }
  implicit def QuotedExprDeco[T](expr: quoted.Expr[T]): QuotedExprAPI

  trait QuotedTypeAPI {
    /** View this expression `Type[T]` as a `TypeTree` */
    def unseal(implicit ctx: Context): TypeTree
  }
  implicit def QuotedTypeDeco[T](tpe: quoted.Type[T]): QuotedTypeAPI

  trait TermToQuotedAPI {
    /** Convert `Term` to an `Expr[Tpe]` with its `Type[Tpe]` for some unknown `Tpe` */
    def seal(implicit ctx: Context): quoted.Sealed
  }
  implicit def TermToQuoteDeco(term: Term): TermToQuotedAPI

  trait SealedAPI {
    /** Return the `expr` as an `Expr[T]` and check that it conforms to a known `T` */
    def asExprOf[T: quoted.Type](implicit ctx: Context): quoted.Expr[T]
  }
  implicit def SealedDeco(seal: quoted.Sealed): SealedAPI
}
