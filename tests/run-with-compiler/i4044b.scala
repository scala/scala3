import scala.quoted._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T]) given QuoteContext: Expr[Unit]
  def expr given QuoteContext: Expr[T]
}

object VarRef {
  def apply[T: Type, U: Type](init: Expr[T])(body: VarRef[T] => Expr[U]) given QuoteContext: Expr[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T]) given QuoteContext: Expr[Unit] = '{ x = $e }
        def expr given QuoteContext: Expr[T] = 'x
      }
    )}
  }

}

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val q = VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })
    println(q.show)
  }
}
