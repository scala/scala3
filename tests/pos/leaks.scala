class Outer1 {
  private val x: Int = 1

  private class Inner {
    def foo: x.type = x // OK
  }
}

object Outer2 {
  private val x: Int = 1
}

class Outer2 {
  private class Inner {
    def foo: Outer2.x.type = Outer2.x // OK
  }
}

class Outer3 {
  private val x: Int = 1

  def meth: Unit = {
    class Inner {
      def foo: x.type = x // OK
    }
  }
}
