package scala.meta

object Reifier {

  def reifyExpr[T](repr: String, args: Seq[Expr[_]]): Expr[T] = ???

  def reifyType[T](repr: String, args: Seq[Type[_]]): Type[T] = ???

}
