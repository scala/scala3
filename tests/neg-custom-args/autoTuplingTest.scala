object autoTupling {

  val x = Some(1, 2)                                  // error when running with -language:noAutoTupling

  x match {
    case Some(a, b) => a + b                          // error // error when running with -language:noAutoTupling
    case None =>
  }
}
