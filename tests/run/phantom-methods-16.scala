
object Test {

  def main(args: Array[String]): Unit = {
    import Foo._
    foo({
      println("bar")
      42
    })
  }
}

object Foo extends Phantom {
  type A <: this.Any

  def foo(x: Int): A = {
    println("foo")
    assume
  }
}
