import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)

    def x: Staged[Int] = '{3}

    def f4: Staged[Int => Int] = '{
      inlineLambda
    }
    println(tb.run(f4(implicitly)(x)))
    println(tb.show(f4(implicitly)(x)))
  }

  inline def inlineLambda <: Int => Int = x => x + x
}