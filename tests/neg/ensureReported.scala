object AnonymousF {
  val f = {
    case l as List(1) => // error: missing parameter type
      Some(l)
  }
}
