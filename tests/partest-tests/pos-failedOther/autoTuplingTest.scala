object autoTupling {

  val x = Some(1, 2)

  x match {
    case Some(a, b) => a + b
    case None =>
  }
}
