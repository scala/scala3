
import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = f(g(Type.IntTag))
    println(q.show)
    '{ println($q) }
  }

  def f(t: Staged[List[Int]])(using QuoteContext): Expr[Int] = '{
    def ff: Int = {
      val a: $t = {
        type T = $t
        val b: T = 3 :: Nil
        b
      }
      a.head
    }
    ff
  }

  def g[T](a: Staged[T])(using QuoteContext): Staged[List[T]] = '[List[$a]]
}
