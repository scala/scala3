

object xfatalWarnings {
  val opt:Option[String] = Some("test")

  opt match { // warn
    case None =>
  }

  object Test {
    while (true) {} // should be ok. no "pure expression does nothing in statement position" issued.
  }
}

// When running with fatal warnings:
