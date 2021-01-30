object Test {
  def main(args: Array[String]): Unit = {
    case class Box(v: Int)

    val x = 42
    var Y1 = 42
    val Y2 = "42"
    var Z1 = Box(4)
    val Z2 = Box(4)

    x match { case Y1                => () } // error
    x match { case Y2.toInt          => () } // error
    x match { case Y1.toString       => () } // error

    x match { case Z1.v              => () } // error
    x match { case Z2.v              => () } // ok

    Some(x) match { case Some(Z1.v)  => () } // error
    Some(x) match { case Some(Z2.v)  => () } // ok

    Some(x) match { case Some(4) | Some(Z1.v) => () } // error
    Some(x) match { case a @ Some(Z1.v)       => () } // error

    Some(x) match { case Some(4) | Some(Z2.v) => () } // ok
    Some(x) match { case a @ Some(Z2.v)       => () } // ok
  }
}
