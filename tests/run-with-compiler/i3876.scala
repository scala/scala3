import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)

    def x: Staged[Int] = '{3}

    def f: Staged[Int => Int] = '{ (x: Int) => x + x }
    println(tb.run(f(implicitly)(x)))
    println(tb.show(f(implicitly)(x)))
  }
}
