// Test that we can't compare for equality `null` with classes.
// This rule is for both regular classes and value classes.

class Foo(x: Int)
class Bar(x: Int) extends AnyVal

class Test {
  locally {
    val foo: Foo = new Foo(15)
    foo == null // error: Values of types Null and Foo cannot be compared
    null == foo // error
    foo != null // error
    null != foo // error

    // To test against null, make the type nullable.
    val foo2: Foo | Null = foo
    // ok
    foo2 == null
    null == foo2
    foo2 != null
    null != foo2
  }

  locally {
    val bar: Bar = new Bar(15)
    bar == null // error: Values of types Null and Foo cannot be compared
    null == bar // error
    bar != null // error
    null != bar // error

    // To test against null, make the type nullable.
    val bar2: Bar | Null = bar
    // ok
    bar2 == null
    null == bar2
    bar2 != null
    null != bar2
  }
}
