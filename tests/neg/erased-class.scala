import language.experimental.erasedDefinitions
import scala.annotation.compileTimeOnly
class AA extends compiletime.Erased
class BB extends AA // ok

@main def Test =
  val f1: Array[AA] = caps.unsafe.unsafeErasedValue // error
  def f2(x: Int): Array[AA] = caps.unsafe.unsafeErasedValue // error
  def bar: AA = caps.unsafe.unsafeErasedValue  // error
  val baz: AA = caps.unsafe.unsafeErasedValue // ok
