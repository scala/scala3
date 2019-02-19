import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(t: Type[Int]): Expr[Unit] = '{}
}
