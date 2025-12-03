// scalajs: --skip
import a._

class T extends A {
  class Acc extends B
  val acc = new Acc
  def foo(): String = acc.foo()
}

@main def Test =
  val t = new T
  println(t.foo())
