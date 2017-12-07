object Test {
  implicit class Foo(i: Int, s: String) // error: Implicit classes must accept exactly one primary constructor parameter
  implicit class Foo0                   // error: Implicit classes must accept exactly one primary constructor parameter
  implicit class Foo1()                 // error: Implicit classes must accept exactly one primary constructor parameter
  implicit class Foo2()(x: Int)         // error: Implicit classes must accept exactly one primary constructor parameter
  implicit case class Bar0              // error: A case class must have at least one parameter list
  implicit case class Bar1()            // error: A case class may not be defined as implicit
  implicit case class Bar2()(x: Int)    // error: A case class may not be defined as implicit
}
