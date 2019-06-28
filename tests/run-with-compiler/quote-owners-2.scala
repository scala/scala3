
import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = f(g(Type.IntTag))
    println(q.show)
    '{ println($q) }
  }

  def f(t: Type[List[Int]]): Expr[Int] = '{
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

  def g[T](a: Type[T]): Type[List[T]] = '[List[$a]]
}
