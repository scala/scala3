
import quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {
  def main(args: Array[String]): Unit = {
    val q = f(g(Type.IntTag))
    println(q.run)
    println(q.show)
  }

  def f(t: Type[List[Int]]): Expr[Int] = '{
    def ff: Int = {
      val a: ~t = {
        type T = ~t
        val b: T = 3 :: Nil
        b
      }
      a.head
    }
    ff
  }

  def g[T](a: Type[T]): Type[List[T]] = '[List[~a]]
}
