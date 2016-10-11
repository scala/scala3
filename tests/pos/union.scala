object Test {

  class A
  class B extends A
  class C extends A
  class D extends A

  val b = true
  val x = if (b) new B else new C
  val y: B | C = x
}
