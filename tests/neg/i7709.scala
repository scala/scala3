
object X with
  protected class Y
object A with
  class B extends X.Y // error
  class B2 extends X.Y: // error
    def this(n: Int) = this()
  class B3(x: Any)
  class B4 extends B3(new X.Y) // error
  class B5(x: String) with
    def this(n: Int) = this(new X.Y().toString) // error
trait T with
  class B extends X.Y // error
class XX with
  protected class Y
class C with
  def xx = new XX
  def y  = new xx.Y  // error
class D with
  def this(n: Int) = {
    this()
    def xx = new XX
    def y  = new xx.Y  // error
  }
class YY extends XX with
  def y = new Y

package p:
  object X with
    protected class Y
  class Q extends X.Y // error
