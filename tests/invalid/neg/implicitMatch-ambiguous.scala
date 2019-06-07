object Test {

  class A
  implicit val a1: A = new A
  implicit val a2: A = new A

  inline def f: Any = implied match {
    case _: A => ???  // error: ambiguous implicits
  }

  f
}