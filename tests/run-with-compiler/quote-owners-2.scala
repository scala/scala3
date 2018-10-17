
import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    def q: Staged[Int] = f(g(Type.IntTag))
    println(tb.run(q))
    println(tb.show(q))
  }

  def f(t: Type[List[Int]]): Staged[Int] = '{
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

  def g[T](a: Type[T])(implicit st: StagingContext): Type[List[T]] = '[List[$a]]
}
