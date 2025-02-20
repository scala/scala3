
class Enumeration:
  protected class Val(i: Int):
    def this() = this(42)
  object Val

class Test extends Enumeration:
  val Hearts = Val(27) // error
  val Diamonds = Val() // error

package p:
  private[p] class C(i: Int) // ctor proxy gets privateWithin of class
  private[p] class D(i: Int)
  object D
  private class E(i: Int)

package q:
  def f() = p.C(42) // error
  def g() = p.D(42) // error
  def h() = p.E(42) // error
