package scala.quoted

class ExprTransformer[T: Type] private (f: PartialFunction[Expr[T], Expr[T]]) {
  def transform[U](e: Expr[U]) given tasty.Reflection: Expr[U] | Expr[T] = {
    val reflect = the[tasty.Reflection]
    import reflect._
//    if (the[quoted.Type[T]].unseal.tpe <:< proto) ... // FIXME use prototype
    if (the[quoted.Type[T]].unseal.tpe <:< e.unseal.tpe)
      f.applyOrElse(e.asInstanceOf[Expr[T]], identity).asInstanceOf[Expr[U]]
    else e
  }
}

object ExprTransformer {
  def apply[T](tpe: Type[T])(f: PartialFunction[Expr[T], Expr[T]]): ExprTransformer[T] = new ExprTransformer(f) given tpe
}
