import scala.quoted._
object Macro {
  rewrite def ff: Unit = ~impl('[Int])
  def impl(t: Type[Int]): Expr[Unit] = '()
}
