import scala.quoted._
type Opt[A] = A | Null
object Opt {
  def f[T](x: T): T = x
  private def mapMacro[A: Type](v: Expr[Opt[A]])(using Quotes): Expr[Unit] =
    '{
      val result: Opt[A] = $v
      if result != null then
        ${f('{result})}
    }
}
