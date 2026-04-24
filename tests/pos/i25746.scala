import scala.language.experimental.subCases

@main def test =
  val r1 = "^foo".r.unanchored
  val r2 = "bar$".r.unanchored

  val result1 = "foo bar" match
    case s @ r1() if s match { case r2() => s }
    case _ => "no"
