import scala.quoted._
import scala.quoted.autolift._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def expr(i: Int): Staged[Int] = '{
      val a = 3 + ${i}
      2 + a
    }
    for (i <- 0 to 200)
      tb.run(expr(i))
  }
}
