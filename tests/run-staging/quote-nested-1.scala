import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    val q = '{ (s: Scope) ?=> '{3} }
    println(q.show)
  }
}
