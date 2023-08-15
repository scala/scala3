import scala.language.experimental.erasedDefinitions

class TakesErased {
  def takesErased(erased x: Int, y: Int): Int = ???
}

@erasedParamsMethod class Foo extends TakesErased

@main def Test() =
  val foo = Foo()
  val v = foo.takesErased(1, 2)
  println(v)
