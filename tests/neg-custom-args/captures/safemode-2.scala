package test
import language.experimental.safe
import caps.unsafe.untrackedCaptures
import scala.annotation.unchecked.{uncheckedCaptures, uncheckedVariance}


object Test:
  val x = caps.unsafe.unsafeErasedValue[String] // error

  @caps.unsafe.untrackedCaptures var y = 1 // error
  @untrackedCaptures def baz() = () // error

  class V[+T] {
    def foo(x: T @uncheckedVariance) // error
      : Array[T @uncheckedVariance] = ??? // error
  }

  def baz(@uncheckedCaptures x: String): Unit = () // error
