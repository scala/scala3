//> using options -Wunused:all

import scala.annotation.unused

class A {
  def f(@unused inUse: Int): Int = inUse // warn
  def g(x: Int, @unused y: Int): Int = y // warn // warn
}
class C(private val x: Int, y: Int, z: Int, @unused w: Int): // warn
  def f(c: C) = c.x
  def g = x + y
