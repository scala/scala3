//> using options -Wunused:all

import scala.annotation.unused

class A {
  def f(@unused inUse: Int): Int = inUse // warn
  def g(x: Int, @unused y: Int): Int = y // warn // warn
}
