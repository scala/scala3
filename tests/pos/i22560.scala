
package companionless:

  class Enumeration:
    protected class Val(i: Int):
      def this() = this(42)

  class Test extends Enumeration:
    val Hearts = Val(27)
    val Diamonds = Val()


package companioned:

  class Enumeration:
    protected class Val(i: Int):
      def this() = this(42)
    protected object Val

  class Test extends Enumeration:
    val Hearts = Val(27)
    val Diamonds = Val()

package p:

  package internal:

    protected[p] class P(i : Int)
    private[p] class Q(i : Int)

  def f = internal.P(42)
  def g = internal.Q(42)
