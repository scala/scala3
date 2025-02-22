
package companionless:

  class Enumeration:
    protected class Val(i: Int):
      def this() = this(42)

package companioned:

  class Enumeration:
    protected class Val(i: Int):
      def this() = this(42)
    protected object Val

package p:

  package internal:

    protected[p] class P(i : Int)
