import scala.annotation.varargs

class Impl extends Abs {
  override def counter(s: String*): Unit = ()
}

trait B extends T {
  override def counter(s: String*): Unit = ()
}

class C extends B {
  override def counter(s: String*) = ()
}

