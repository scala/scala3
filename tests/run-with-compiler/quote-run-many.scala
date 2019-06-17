import scala.quoted._
import scala.quoted.autolift._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def expr(i: Int) = '{
      val a = 3 + ${i}
      2 + a
    }
    for (i <- 0 to 200)
      run {
        expr(i)
      }
  }
}
