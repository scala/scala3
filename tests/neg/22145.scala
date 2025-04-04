package foo

trait Collection:
  val base: Collection = ???
  base.foo() // error

  object O extends Collection:
    def foo(): Int = ???
