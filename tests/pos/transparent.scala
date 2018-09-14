class Foo {
  rewrite def foo() = {
    abstract class C[T] extends Object {
      def x: T
      println(x)
    }
    ()
  }
}
