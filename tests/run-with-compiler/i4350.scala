
import scala.quoted._

class Foo[T: Type] {
  def q = '{(null: Any).asInstanceOf[T]}
}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    println(show((new Foo[Object]).q))
    println(show((new Foo[String]).q))
  }
}
