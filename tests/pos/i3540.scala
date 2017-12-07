object Test {
  implicit class Foo(x: Int)(implicit y: Int)
  implicit class Foo0(x: Int)(y: Int)(implicit z: Int) // OK but not used during implicit lookup
  implicit class Foo1(x: Int)(y: Int)                  // OK but not used during implicit lookup
}
