object AnonymousF {
  val f = {
    case List(1) as l => // error: missing parameter type
      Some(l)
  }
}
