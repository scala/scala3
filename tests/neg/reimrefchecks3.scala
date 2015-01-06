import dotty.readonly
import dotty.mutable

object reimrefchecks3 {
  class A {
    def m() = 3
    @readonly def n() = 3
    val a = new A
    a.m() // OK
    a.n() // OK
    val roa = a : @readonly
    roa.m() // ERROR
    roa.n() // OK
  }
}