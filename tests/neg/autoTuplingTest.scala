import dotty.language.noAutoTupling

object autoTuplingNeg {

  val x = Some(1, 2)                                  // error

  x match {
    case Some(a, b) => a + b                          // error // error // error
    case None =>
  }
}
