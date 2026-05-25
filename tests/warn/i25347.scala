//> using options -deprecation -feature

package p

import scala.language.implicitConversions

trait T {
  object i {
    class C
    private[p] object C:
      given c2i: Conversion[C, Int] = _ => 42
  }
  object j {
    class D
    private[j] object D:
      given d2i: Conversion[D, Int] = _ => 42
  }
  class E
  protected object E:
    implicit def e2i(a: E): Int = 42
}

object Test extends T {
  def t1: Int = {
    val c = new i.C()
    c // ok
  }
  def t2: Int = {
    val d = new j.D()
    d // warn
  }
  def t3: Int = {
    val e = new E()
    e // ok
  }
  def t4: Int = {
    val t = new T {}
    val e = new t.E()
    e // warn
  }
}
