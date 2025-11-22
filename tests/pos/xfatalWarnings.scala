//> using options -nowarn -Werror
// succeeds despite -Xfatal-warnings because of -nowarn

object xfatalWarnings {
  val opt:Option[String] = Some("test")

  opt match { // error when running with -Xfatal-warnings
    case None =>
  }

  object Test {
    while (true) {} // should be ok. no "pure expression does nothing in statement position" issued.
  }
}
