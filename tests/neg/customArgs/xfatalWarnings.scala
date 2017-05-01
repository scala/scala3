object xfatalWarnings {
  val opt:Option[String] = Some("test")

  opt match { // error when running with -Xfatal-warnings
    case None =>
  }
}