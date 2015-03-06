import dotty.readonly
import dotty.mutable

object reimrefchecks4 {
  class A {
    def m() = 3
    @readonly def n() = m() // ERROR
  }
}
