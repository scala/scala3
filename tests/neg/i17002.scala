import scala.annotation.compileTimeOnly

sealed trait Test[T]

object Test:
  @compileTimeOnly("Error")
  given test0[T]: Test[T] = ???

  @compileTimeOnly("Error")
  given test1[T]: Test[T]()

  @compileTimeOnly("Error")
  implicit class ic(x: Int):
    def foo = 2

  test0 // error

  test1 // error

  2.foo // error