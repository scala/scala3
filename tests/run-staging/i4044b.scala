import scala.quoted._
import scala.quoted.staging._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T])(using QuoteContext): Expr[Unit]
  def expr(using QuoteContext): Expr[T]
}

object VarRef {
  def apply[T: Staged, U: Staged](init: Expr[T])(body: VarRef[T] => Expr[U])(using QuoteContext): Expr[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T])(using QuoteContext): Expr[Unit] = '{ x = $e }
        def expr(using QuoteContext): Expr[T] = 'x
      }
    )}
  }

}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val q = VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })
    println(q.show)
  }
}
