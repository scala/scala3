import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = usingNewScope {
    val q = '{(using s: Scope) =>
      val a = '{4}
      '{${a}}
    }

    println(q.show)
  }
}
