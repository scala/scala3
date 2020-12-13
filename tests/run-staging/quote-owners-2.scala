
import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = f(g(Type.of[Int]))
    println(q.show)
    '{ println($q) }
  }

  def f(t: Type[List[Int]])(using Quotes): Expr[Int] = '{
    def ff: Int = {
      val a: t.Underlying = {
        type T = t.Underlying
        val b: T = 3 :: Nil
        b
      }
      a.head
    }
    ff
  }

  def g[T](a: Type[T])(using Quotes): Type[List[T]] = Type.of[List[a.Underlying]]
}
