import scala.annotation.varargs

abstract class NoAnnot {
  // java varargs forwarder: no
  def f(args: String*): Unit
}
class B1 extends NoAnnot {
  // java varargs forwarder: no
  override def f(args: String*) = ()
}
class B2 extends NoAnnot {
  // java varargs forwarder: yes, but it doesn't override anything
  @varargs
  override def f(args: String*) = ()
}
class C1 extends B2 {
  // java varargs forwarder: yes, overrides parent forwarder
  override def f(args: String*) = ()
}
class C2 extends B2 {
  // java varargs forwarder: yes, overrides parent forwarder
  @varargs
  override def f(args: String*) = ()
}
