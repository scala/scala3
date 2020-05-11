import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(using s: Scope)(t: s.Type[Int]): s.Expr[Unit] = '{}
}
