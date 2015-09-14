object Test extends dotty.runtime.LegacyApp {
  case class A()
  class B extends A() { override def toString() = "B()" }
  println(A())
  println(new B())
}

