
class Foo {
  // Test that prototypes of the form "SomeType | Null" are used in type inference.
  // Otherwise, the application of `Array` below would be typed as `Array.apply[String](x)`,
  // which doesn't typecheck.
  def foo(x: Array[String|Null]|Null) = 0
  foo(Array("hello"))
}
