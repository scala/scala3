import scala.quoted._

object Position {
  def withPosition[T](fun: Expr[Unit => T])(using quotes: Quotes, typeOfT: Type[T]): Expr[T] =
    '{${fun}.apply(null)}
}
