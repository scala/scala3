import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {

    val q = '{(using s: Scope) =>
      def a(using Scope) = '{4}
      ${'{(using s2: Scope) =>
        '{${a}}
      }}

    }

    println(q.show)
  }
}
