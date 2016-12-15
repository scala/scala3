class Foo {
  class B(val a: Int) extends AnyVal // error: value class may not be a member of another class`
}

object Test {
  class B1(val b: Int) extends B(b) // error: cannot extend final class B
}


