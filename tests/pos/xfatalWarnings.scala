// scalac: -nowarn -Werror
// succeeds despite -Werror because of -nowarn
object xfatalWarnings {
  val opt:Option[String] = Some("test")

  opt match { // error when running with -Werror
    case None =>
  }

  object Test {
    while (true) {} // should be ok. no "pure expression does nothing in statement position" issued.
  }
}
