trait T {
  object O
}

class C extends T

object Test {

  val c = new C
  c.O

}
