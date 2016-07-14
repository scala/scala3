object Test {
  trait A
  trait B
  abstract sealed class Parent
  class Foo extends Parent with A
  class Bar extends Parent with B

  (null: Parent) match {
    case (_: A) | (_: B) =>
      /*
       * This case would incorrectly be reported as an error,
       * due to a typo in IsInstanceOfEvaluator
       */
  }
}
