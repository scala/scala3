
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
