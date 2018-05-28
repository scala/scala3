import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted.Type

class Foo[T: Type] {
  def q = '(null.asInstanceOf[T])
}

object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo[Object]).q.show)
    println((new Foo[String]).q.show)
  }
}
