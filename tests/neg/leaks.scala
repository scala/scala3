class Outer {
  private val x: Int = 1

  class Inner {
    def foo: x.type = x // error: non-private method foo refers to private value x in its type signature
  }
}
