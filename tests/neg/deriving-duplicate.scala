object Test extends App {

  class A {
    class TC1[T] {
    }
    object TC1 {
      def derived[T]: TC1[T] = new TC1[T]
    }
  }
  class A2 {
    class TC1[T] {
    }
    object TC1 {
      def derived[T]: TC1[T] = new TC1[T]
    }
  }
  val a = new A
  val a2 = new A2

  case class D() derives a.TC1, a2.TC1  // error: duplicate type class derivation
}