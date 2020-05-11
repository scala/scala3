import scala.quoted._
import scala.quoted.staging._

sealed abstract class VarRef[T] {
  val outer: Scope
  def update(using s: outer.Nested)(expr: s.Expr[T]): s.Expr[Unit]
  def expr(using s: outer.Nested): s.Expr[T]
}

object VarRef {
  def apply[T, U](using s: Scope)(init: s.Expr[T])(body: VarRef[T] { val outer: s.type } => s.Expr[U])(using s.Type[T], s.Type[U]): s.Expr[U] = '{
    var x = $init
    ${body(
      new VarRef[T] {
        val outer: s.type = s
        def update(using s2: outer.Nested)(e: s2.Expr[T]): s2.Expr[Unit] = '{ x = $e }
        def expr(using s2: outer.Nested): s2.Expr[T] = 'x
      }
    )}
  }

}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    val q = VarRef('{4})(varRef => '{ ${varRef.update('{3})}; ${varRef.expr} })
    println(q.show)
  }
}
