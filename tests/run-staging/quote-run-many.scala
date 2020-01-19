import scala.quoted._
import scala.quoted.staging._
import scala.quoted.autolift.{given _}

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def expr(i: Int) with QuoteContext = '{
      val a = 3 + ${i}
      2 + a
    }
    for (i <- 0 to 100)
      run(expr(i))
  }
}
