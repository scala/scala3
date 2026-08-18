case class Foo(arg1: Int, arg2: Int)
case class Bar(arg1: Int, arg2: Option[Int])

object OK:
  Foo(1, 2) match
    case Foo(arg1 =
    5
    ) =>

object Test:
  Foo(1, 2) match
    case Foo(arg1 =
      5 // was: error: pattern expected
    ) =>

object Nest:
  Bar(1, Option(2)) match
    case Bar(arg2 =
      Some(value =
        42
      )
    ) =>

object Detupled:
  (fst = 1, snd = 2) match
    case (fst =
      5
    ) =>
