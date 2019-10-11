package scala.tasty.reflect

import scala.quoted._

/** Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects */
trait QuotedOps extends Core {

  implicit class QuotedExprAPI[T](expr: Expr[T]) {
    /** View this expression `scala.quoted.Expr[T]` as a `Term` */
    def unseal(given ctx: Context): Term =
      internal.QuotedExpr_unseal(expr)

    /** Checked cast to a `scala.quoted.Expr[U]` */
    def cast[U: TypeTag](given ctx: Context): Expr[U] = // TODO use TypeTag after rebootstrap
      internal.QuotedExpr_cast[U](expr)
  }

  implicit class QuotedTypeAPI[T <: AnyKind](tpe: TypeTag[T]) {
    /** View this expression `scala.quoted.TypeTag[T]` as a `TypeTree` */
    def unseal(given ctx: Context): TypeTree =
      internal.QuotedType_unseal(tpe)
  }

  implicit class TermToQuotedAPI(term: Term) {
    /** Convert `Term` to an `scala.quoted.Expr[Any]` */
    def seal(given ctx: Context): Expr[Any] =
      internal.QuotedExpr_seal(term)
  }

  implicit class TypeToQuotedAPI(tpe: Type) {
    /** Convert `Type` to an `scala.quoted.TypeTag[_]` */
    def seal(given ctx: Context): TypeTag[_] =
      internal.QuotedType_seal(tpe)
  }
}
