object Test {

  class A
  class B extends A
  class C extends A

  val b = true

  val x: B | C = if (b) new B else new C

  val y:  B | C = x
  val ok: B | C = y  // ok, as x and y are ascribed with unions

  val z = x
  val error: B | C = z // error, as z is not ascribed with an union
}
