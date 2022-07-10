object Foo:
  inline def unapply(using String)(i: Int): Some[Int] = Some(i)

given String = ""

val i = 10 match
  case Foo(x) => x // error
