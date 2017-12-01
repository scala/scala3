class Foo {
  val a: Any = 3
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}

class Bar[T] {
  val a: T = ???
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}

class Baz {
  val a: Double = 1.0
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}

class Foo2 {
  val a: AnyVal = 3
  a match {
    case 1 =>
    case 2 =>
    case 3 =>
    case _ =>
  }
}


case class A(i: Int) extends AnyVal
class Foo3 {
  val a: A = new A(3)
  a match {
    case A(1) =>
    case A(2) =>
    case A(3) =>
    case _ =>
  }
}
