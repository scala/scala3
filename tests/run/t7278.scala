class Foo {
    class Bar {
      def foo = 1
    }
  }

  class SubFoo extends Foo {
    class SubBar {
      def foo = 42
    }
  }

object Test {
  def main(args: Array[String]): Unit = {

// Let's create some instances:
  val foo = new Foo
  val fooBar = new foo.Bar
  assert(fooBar.foo == 1)                         //> res0: Int = 1
  // ok

  val subFoo = new SubFoo
  val subFooBar = new subFoo.SubBar
  assert(subFooBar.foo == 42)                      //> res1: Int = 42
  // ok

  val superFoo: Foo = subFoo
  val superFooBar = new superFoo.Bar
  assert(superFooBar.foo == 1)                     //> res2: Int = 1
  // NOT OK!
  }
}
