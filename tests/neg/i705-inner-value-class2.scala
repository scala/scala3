class Foo {
  class B(val a: Int) extends AnyVal
}

object Test {
  class B1(val b: Int) extends B(b) // error: cannot extend final class B
}


