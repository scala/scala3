object Foo:
  inline def unapply(using String)(i: Int): Some[Int] = Some(i)

object Bar:
  inline def unapply(using String)(using String)(i: Int): Some[Int] = Some(i)

object Baz:
  inline def unapply[T](using String)(i: T): Some[T] = Some(i)

given String = ""

val i = 10 match
  case Foo(x) => x
  case Bar(x) => x
  case Baz(x) => x
