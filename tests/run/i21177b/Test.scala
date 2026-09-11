abstract class B { def m(): Int }
trait T extends B with A
class C extends T

object Test {
  def main(args: Array[String]): Unit = {
    val cls = classOf[C]
    assert(cls.getInterfaces().length == cls.getGenericInterfaces().length)
    new C().m() // lack of the mixin forwarder for m() will crash this on runtime
  }
}
