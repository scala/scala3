import quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {

    val q = '{
      val t = '[String]
      t
    }

    println(q.show)
  }
}
