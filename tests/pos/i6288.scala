object A {
  opaque type T = 3
  object T {
    val x: T = 3
  }
}

object O {
  val x = 3
  opaque type T = x.type
  object T {
    def wrap(a: x.type): T = a   // was an error, now OK
    def unwrap(a: T): x.type = a // OK
  }
}