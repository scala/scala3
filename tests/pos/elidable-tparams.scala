import annotation.*
import elidable.*

class ElidableCrashTest {
  trait My

  @elidable(MINIMUM) def foo[a >: My <: My]: scala.Unit = ()

  foo[My] // crash
}
