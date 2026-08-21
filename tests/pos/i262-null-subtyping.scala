object O {
  trait Base extends Any { type T }
  val a: Base { type T } = null;
  val b: Any & Base { type T } = null;

  val c: AnyRef & Base { type T } = null;

  class A
  class B

  val d: A & B = null
  val e: A | B = null

  val f: (A & B) { def toString: String } = null
  val g: (A | B) { def toString: String } = null
}
