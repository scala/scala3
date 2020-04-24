val (x, y) =
  try {
    ???
    (1, 2)
  }
  catch {
    case e if !true =>
      val x = 2
      val foo = e match {
        case e: java.io.IOException => ???
      }
      (1, 2)
  }
