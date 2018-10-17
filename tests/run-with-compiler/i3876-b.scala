import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)

    def x: Staged[Int] = '{3}

    def f2: Staged[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }
    println(tb.run(f2(implicitly)(x)))
    println(tb.show(f2(implicitly)(x)))
  }
}
