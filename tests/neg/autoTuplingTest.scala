import language.noAutoTupling

object autoTuplingNeg2 {

  val x = Some(1, 2) // error: too many arguments for method apply: (x: A)Some[A]

  x match {
    case Some(a, b) => a + b // error: wrong number of argument patterns for Some // error: not found: b
    case None =>
  }
}
