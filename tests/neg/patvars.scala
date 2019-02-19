object Test {

  (??? : Any) match {

    case x1 | y1 => ???  // error // error

    case _: List[t2] | y2 => ??? // error // error

    case x3: Int | y3: String => ??? // error // error

  }
}