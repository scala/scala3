
package companionless:

  class Test extends Enumeration:
    val Hearts = Val(27)
    val Diamonds = Val()


package companioned:

  class Test extends Enumeration:
    val Hearts = Val(27)
    val Diamonds = Val()

package p:

  def f = new internal.P(42)
