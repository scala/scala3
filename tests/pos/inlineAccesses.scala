trait T {
  object O
}

class C {
  private type T = C
  private var x = 0

  transparent def f = {
    x += 1
    x = x + 1
    x
  }
  transparent def dup = new T
}

object Test {

  val c = new C
  c.f
  c.dup
}
