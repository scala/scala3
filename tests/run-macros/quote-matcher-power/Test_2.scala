import Macros.*

object Test {

  def main(args: Array[String]): Unit = {
    val x = 2
    println(power(x, 5))

    println(rewrite{ power2(2.0, 5.0) * power2(2.0, 4.0) })

    println(rewrite{ power2(2.0, 1.0) })

    println(rewrite{ power2(1.0, 1000) })

    println(rewrite{ power2(power2(2.0, 2.0), 3.0) })
  }
}
