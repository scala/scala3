import scala.quoted._
import scala.quoted.staging._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T]) with QuoteContext : Expr[Unit]
  def expr with QuoteContext : Expr[T]
}

object VarRef {
  def apply[T: Type, U: Type](init: Expr[T])(body: VarRef[T] => Expr[U]) with QuoteContext : Expr[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T]) with QuoteContext : Expr[Unit] = '{ x = $e }
        def expr with QuoteContext : Expr[T] = 'x
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
