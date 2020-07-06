// test printing of private[D] and protected[D]

package O
package A

class D {
  class C {
    protected[D] def a: Int = 0
    private[D]   def b: Int = 0
    private      def c: Int = 0
    protected    def d: Int = 0
    private[A]   def e: Int = 0
    protected[A] def f: Int = 0
                 def g: Int = 0
                 def g1(t: Int) = 0
                 extension (c: C) def g1 = 0
  }

  private[D]      class E
  private         class F
  private[A]      class G
  protected[D]    class H
  protected       class I
  protected[A]    class J
  /* public */    class K

  protected[D] val a: Int = 0
  private[D]   val b: Int = 0
  private      val c: Int = 0
  protected    val d: Int = 0
  private[A]   val e: Int = 0
  protected[A] val f: Int = 0
               val g: Int = 0
}
