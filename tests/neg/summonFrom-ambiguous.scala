object Test {

  class A
  implicit val a1: A = new A
  implicit val a2: A = new A

  inline def f: Any = compiletime.summonFrom {
    case _: A => ???
  }

  f // error: ambiguous implicits
}