object Test extends App {

  class A {
    class TC1[T] {
      def print() = println("a.TC1")
    }
    object TC1 {
      def derived[T]: TC1[T] = new TC1[T]
    }
  }
  class A2 {
    class TC1[T] {
      def print() = println("a2.TC1")
    }
    object TC1 {
      def derived[T]: TC1[T] = new TC1[T]
    }
  }
  class B {
    class TC2[T] {
      def print() = println("b.TC2")
    }
    object TC2 {
      def derived[T]: TC2[T] = new TC2[T]
    }
  }
  val a = new A
  val a2 = new A2
  val b = new B

  case class C() derives a.TC1, b.TC2

  implicitly[a.TC1[C]].print()
  implicitly[b.TC2[C]].print()

  type TC1a[T] = a2.TC1[T]
  case class D() derives a.TC1, TC1a

  implicitly[a.TC1[D]].print()
  implicitly[a2.TC1[D]].print()
}