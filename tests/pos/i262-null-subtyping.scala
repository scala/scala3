object O {
  trait Base extends Any { type T }
  val a: Base { type T } = ???;
  val b: Any & Base { type T } = ???;

  val c: AnyRef & Base { type T } = ???;

  class A
  class B

  val d: A & B = ???
  val e: A | B = ???

  val f: (A & B) { def toString: String } = ???
  val g: (A | B) { def toString: String } = ???
}
