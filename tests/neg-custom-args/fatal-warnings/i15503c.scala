// scalac: -Wunused:privates
trait C
class A:
  self: C => // OK
  class B:
    private[A] val a = 1 // OK
    private[B] val b = 1 // OK
    private[this] val c = 1 // error
    private val d = 1 // error

    private[A] val e = 1 // OK
    private[this] val f = e // OK
    private val g = f // OK

    val x = 1 // OK
    def y = 2 // OK
    def z = g // OK