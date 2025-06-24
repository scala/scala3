import language.experimental.erasedDefinitions
import scala.annotation.compileTimeOnly
erased class AA
erased class BB extends AA // ok

@main def Test =
  val f1: Array[AA] = caps.unsafe.unsafeErasedValue // error // error
  def f2(x: Int): Array[AA] = caps.unsafe.unsafeErasedValue // error // error
  def bar: AA = caps.unsafe.unsafeErasedValue  // ok
  val baz: AA = caps.unsafe.unsafeErasedValue // ok
