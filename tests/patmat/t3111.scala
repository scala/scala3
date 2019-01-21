object Test {
  val bool: Boolean = false

  bool match {
    case true => ()
  }

  bool match {
    case true => ()
    case false => ()
    case _ => ()
  }
}