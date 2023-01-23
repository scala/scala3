
package top {
  package middle {
    class C {
      def c() = println("hello, world")
    }
    import Predef.{Map => _}
    object Test {
      def f() = Map("hello" -> "world") // error // error
      def g() = println(f()) // error
    }
  }
}
