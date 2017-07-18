class Cont[A0](x0: A0) { type A = A0; val x: A = x0 }
object Test {
  val c: { type A; val x: A } & { type A = Int } = new Cont(1)
  println(c.x : Int) // error: not an instance of Selectable
}