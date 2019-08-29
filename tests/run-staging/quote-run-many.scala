import scala.quoted._
import scala.quoted.staging._
import given scala.quoted.autolift._

object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
    def expr(i: Int) given QuoteContext = '{
      val a = 3 + ${i}
      2 + a
    }
    for (i <- 0 to 100)
      run(expr(i))
  }
}
