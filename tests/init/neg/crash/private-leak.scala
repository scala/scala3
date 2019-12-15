package private_leak

class Test {
  private type Foo = Int

  val x: Foo = 1

  x // error
}
