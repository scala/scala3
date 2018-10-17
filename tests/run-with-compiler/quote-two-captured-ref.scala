import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    println(tb.show('{
      val x = 1
      println(${
        println(1)
        val a = 'x
        val b = 'x
        '{ $a + $b }
      })
    }))
  }
}
