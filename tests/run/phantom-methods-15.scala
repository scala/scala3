
object Test {

  def main(args: Array[String]): Unit = {
    println("normal println")
    import Foo._
    a()
    b()
    Foo.a()
    Foo.b()
    c
    d
    Foo.c
    Foo.d
  }
}

object Foo extends Phantom {
  type A <: this.Any

  def a(): A = {
    println("erased println in phantom")
    assume
  }

  def b[X <: A](): X = {
    println("erased println in phantom")
    assume
  }

  def c: A = {
    println("erased println in phantom")
    assume
  }

  def d[X <: A]: X = {
    println("erased println in phantom")
    assume
  }
}
