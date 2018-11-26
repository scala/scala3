
class Foo {

  // Test that the compiler can fill in an implicit argument
  // of the form `T|Null`.
  def foo(implicit s: String|Null) = s

  {
    implicit val arg1: String = "hello"
    foo
  }

  {
    implicit val arg2: Null = null
    foo
  }
}
