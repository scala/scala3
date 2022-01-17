import language.experimental.erasedDefinitions
import scala.annotation.compileTimeOnly
erased class AA
erased class BB extends AA // ok

@main def Test =
  val f1: Array[AA] = compiletime.erasedValue // error // error
  def f2(x: Int): Array[AA] = compiletime.erasedValue // error // error
  def bar: AA = compiletime.erasedValue  // ok
  val baz: AA = compiletime.erasedValue // ok
