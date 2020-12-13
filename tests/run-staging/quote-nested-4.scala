import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {

    val q = '{ (using q: Quotes) =>
      val t = Type.of[String]
      t
    }

    println(q.show)
  }
}
