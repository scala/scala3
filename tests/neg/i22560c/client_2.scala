
package i22560:
  val alpha = C.D() // error

  class Test extends Enumeration:
    val Hearts = Val(27) // error
    val Diamonds = Val() // error

package q:
  def f() = p.C(42) // error
  def g() = p.D(42) // error
