object Test {

  None match {
    case Some(0) => ??? // error: unreachable
  }

}
