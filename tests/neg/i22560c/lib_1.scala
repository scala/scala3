
package i22560:

  object C:
    protected class D

  class Enumeration:
    protected class Val(i: Int):
      def this() = this(42)
    object Val

package p:
  private[p] class C(i: Int) // companion gets privateWithin of class
  private[p] class D(i: Int) // ctor proxy gets privateWithin of class
  object D

