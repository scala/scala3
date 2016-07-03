object Test {
  sealed trait A
  sealed trait B

  abstract sealed class Parent
  class Foo extends Parent with A with B
  class Bar extends Parent with B with A

  (null: A) match {
    case _: B =>
  }
}
