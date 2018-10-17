import scala.quoted._

sealed abstract class VarRef[T] {
  def update(expr: Expr[T]): Staged[Unit]
  def expr: Staged[T]
}

object VarRef {
  def apply[T: Type, U: Type](init: Expr[T])(body: VarRef[T] => Expr[U]): Staged[U] = '{
    var x = $init
    ${body(
      new VarRef {
        def update(e: Expr[T]): Staged[Unit] = '{ x = $e }
        def expr: Staged[T] = 'x
      }
    )}
  }

}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val tb = Toolbox.make
    println(tb.show(VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })))
  }
}
