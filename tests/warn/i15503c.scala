//> using options -Wunused:privates -source:3.3

trait C
class A:
  self: C => // OK
  class B:
    private[A] val a = 1 // OK
    private[B] val b = 1 // OK
    private[this] val c = 1 // warn
    private val d = 1 // warn

    private[A] val e = 1 // OK
    private[this] val f = e // OK
    private val g = f // OK

    private[A] var h = 1 // OK
    private[this] var i = h // warn not set
    private var j = i // warn not set

    private[this] var k = 1 // OK
    private var l = 2 // OK
    private val m = // warn
      k = l
      l = k
      l

    private def fac(x: Int): Int = // warn
      if x == 0 then 1 else x * fac(x - 1)

    val x = 1 // OK
    def y = 2 // OK
    def z = g // OK
    var w = 2 // OK

package foo.test.constructors:
  case class A private (x: Int) // OK
  class B private (val x: Int) // OK
  object B { def default = B(42) }
  class C private (private val xy: Int) // warn
  object C { def default = C(42) }
  class D private (private val x: Int): // OK
    def y = x
  object D { def default = D(42) }
  class E private (private var x: Int): // warn not set
    def y = x
  object E { def default = E(42) }
  class F private (private var x: Int): // OK
    def y =
      x = 3
      x
  object F { def default = F(42) }

package test.foo.i16682:
  object myPackage:
    private object IntExtractor: // OK
        def unapply(s: String): Option[Int] = s.toIntOption

    def isInt(s: String) = s match {
        case IntExtractor(i) => println(s"Number $i")
        case _ => println("NaN")
    }

  def f = myPackage.isInt("42")

object LazyVals:
  import java.util.concurrent.CountDownLatch

  // This trait extends Serializable to fix #16806 that caused a race condition
  sealed trait LazyValControlState extends Serializable

  final class Waiting extends CountDownLatch(1), LazyValControlState:
    private def writeReplace(): Any = null
