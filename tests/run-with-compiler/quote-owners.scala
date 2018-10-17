import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    def q: Staged[Int] = f
    println(tb.run(q))
    println(tb.show(q))
  }

  def f: Staged[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g: Staged[Int] = '{
    val a = 9
    a + 0
  }
}
