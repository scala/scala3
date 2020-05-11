
import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = f(g('[Int]))
    println(q.show)
    '{ println($q) }
  }

  def f(using s: Scope)(t: s.Type[List[Int]]): s.Expr[Int] = '{
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

  def g[T](using s: Scope)(a: s.Type[T]): s.Type[List[T]] = '[List[$a]]
}
