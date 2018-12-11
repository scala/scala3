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
    /** Convert `Term` to an `Expr[T]` and check that it conforms to `T` */
    def seal[T: scala.quoted.Type](implicit ctx: Context): scala.quoted.Expr[T]
  }
  implicit def TermToQuoteDeco(term: Term): TermToQuotedAPI

}
