import scala.quoted._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T]): Expr[Unit]
  def expr: Expr[T]
}

object VarRef {
  def apply[T: Type, U: Type](init: Expr[T])(body: VarRef[T] => Expr[U]): Expr[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T]): Expr[Unit] = '{ x = $e }
        def expr: Expr[T] = 'x
      }
    )}
  }

}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val q = VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })
    println(q.show)
  }
}
