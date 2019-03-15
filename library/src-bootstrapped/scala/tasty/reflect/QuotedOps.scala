package scala.tasty.reflect

/** Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects */
trait QuotedOps extends Core {

  implicit class QuotedExprAPI[T](expr: scala.quoted.Expr[T]) {
    /** View this expression `Expr[T]` as a `Term` */
    def unseal(implicit ctx: Context): Term =
      kernel.QuotedExpr_unseal(expr)
  }

  implicit class QuotedTypeAPI[T <: AnyKind](tpe: scala.quoted.Type[T]) {
    /** View this expression `Type[T]` as a `TypeTree` */
    def unseal(implicit ctx: Context): TypeTree =
      kernel.QuotedType_unseal(tpe)
  }

  implicit class TermToQuotedAPI(term: Term) {
    /** Convert `Term` to an `Expr[T]` and check that it conforms to `T` */
    def seal[T](implicit tpe: scala.quoted.Type[T], ctx: Context): scala.quoted.Expr[T] =
      kernel.QuotedExpr_seal(term)(tpe)
  }

  implicit class TypeToQuotedAPI(tpe: Type) {
    /** Convert `Type` to an `quoted.Type[T]` */
    def seal(implicit ctx: Context): scala.quoted.Type[_] =
      kernel.QuotedType_seal(tpe)
  }
}
