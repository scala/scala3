// Test that nullability is correctly detected
// in the presence of a type alias.

class Base {
  type T >: Null <: AnyRef|Null
}

object foo {
  class Foo {
    val length: Int = 42
    def doFoo(): Unit = ()
  }
}

class Derived extends Base {
  type Nullable[X] = X | Null
  type Foo = Nullable[foo.Foo]

  def fun(foo: Foo): Unit = {
    foo.length  // error: foo is nullable
    foo.doFoo() // error: foo is nullable
  }
}
