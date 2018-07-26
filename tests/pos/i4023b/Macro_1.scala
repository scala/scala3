import scala.quoted._
object Macro {
  transparent def ff[T](implicit t: Type[T]): Int = ~impl[T]
  def impl[T]: Expr[Int] = '(4)
}
