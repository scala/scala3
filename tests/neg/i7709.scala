
object X:
  protected class Y
object A:
  class B extends X.Y // error
  class B2 extends X.Y: // error
    def this(n: Int) = this()
  class B3(x: Any)
  class B4 extends B3(new X.Y) // error
  class B5(x: String):
    def this(n: Int) = this(new X.Y().toString) // error
trait T:
  class B extends X.Y // error
class XX:
  protected class Y
class C:
  def xx = new XX
  def y  = new xx.Y  // error
class D:
  def this(n: Int) = {
    this()
    def xx = new XX
    def y  = new xx.Y  // error
  }
class YY extends XX:
  def y = new Y

package p:
  object X:
    protected class Y
  class Q extends X.Y // error
