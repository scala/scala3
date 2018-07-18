import scala.quoted._
object Macro {
  transparent def ff: Unit = ~impl('[Int])
  def impl(t: Type[Int]): Expr[Unit] = '()
}
