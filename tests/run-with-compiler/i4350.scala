
import scala.quoted._

class Foo[T: Type] {
  def q: Staged[T] = '{(null: Any).asInstanceOf[T]}
}

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    println(tb.run(new Foo[Object]().q.show.toExpr))
    println(tb.run(new Foo[String]().q.show.toExpr))
  }
}
