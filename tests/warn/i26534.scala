//> using options -Wunused:all

import scala.annotation.unused

class A {
  def f(@unused inUse: Int): Int = inUse // warn
  def g(x: Int, @unused y: Int): Int = y // warn // warn
}
class C(private val x: Int, y: Int, z: Int, @unused w: Int): // warn
  def f(c: C) = c.x
  def g = x + y
class D:
  @unused private val p =
    @unused val q = 41
    42

class E:
  @unused private var v = 42 // ok, would have warned unassigned
  @unused private var w = 27 // ok, would have warned unreferenced
  @unused private var x = 27 // warn unused unused, x is read and assigned
  def f = v
  def g() = w = 5
  def h =
    x = x + 1
    x

class F:
  def f(using s: String) = println(42) // warn
  def g(using @unused s: String) = println(42)

// more helpful to warn unused explicit parameter (and not about var or private member)
class C9a(private[this] var x: Int) // warn // warn was unused explicit parameter
class C9b(private var x: Int) // warn was unused private member

def method(): Unit =
  var local = 42 // warn unused local
  ()
