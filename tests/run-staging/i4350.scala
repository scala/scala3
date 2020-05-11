
import scala.quoted._
import scala.quoted.staging._

class Foo[T](using val s: Scope)(using s.Type[T]) {
  def q = '{(null: Any).asInstanceOf[T]}
}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    val a = new Foo[Object]
    val b = new Foo[String]
    println(a.q.show)
    println(b.q.show)
  }
}
