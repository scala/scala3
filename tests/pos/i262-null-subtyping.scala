object O {
  // This compiles
  val a: { type T } = null;
  val b: Any { type T } = null;

  // This doesn't:
  // found   : Null
  // required: AnyRef{T}
  val c: AnyRef { type T } = null;

  class A
  class B

  val d: A & B = null
  val e: A | B = null

  val f: (A & B) { def toString: String } = null
  val g: (A | B) { def toString: String } = null
}
