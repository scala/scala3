object Foo {
  object Bar
  implicit class Bar // error: Implicit classes must accept exactly one primary constructor parameter
}
