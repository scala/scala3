  trait A:
    self: A =>
    def foo: Int

  abstract class B extends A:
    def foo: Int

  class C extends B:
    def foo = 1
    def derived = this
