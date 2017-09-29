sealed class Foo

class Test {
  def f = {
    class Bar extends Foo
  }
  class C {
    class Bar extends Foo
  }
  object O {
    class Bar extends Foo
  }
}

