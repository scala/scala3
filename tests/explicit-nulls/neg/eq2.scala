
// Test that we can't compare for equality `null` and
// classes that derive from AnyVal.
class Foo(x: Int) extends AnyVal

class Bar {
  val foo: Foo = new Foo(15)
  if (foo == null) {} // error
  if (null == foo) {} // error

  // To test against null, make the type nullable.
  val foo2: Foo|Null = foo
  if (foo2 == null) {} 
  if (null == foo2) {}
}
