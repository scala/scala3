import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {

    val q = '{(q: Quotes) ?=>
      val a = '{4}
      ${'{(q2: Quotes) ?=>
        '{${a}}
      }}

    }

    println(q.show)
  }
}
