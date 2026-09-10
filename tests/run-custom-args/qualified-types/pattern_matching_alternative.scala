@main def Test =
  val x: Any = "Hello"

  x match
    case (_: Int) | (_: {s: String with s.nonEmpty}) =>
      println(s"$x is an Int or a non-empty String")
    case _ =>
      ()

  x match
    case _: (Int | {s: String with s.nonEmpty}) =>
      println(s"$x is an Int or a non-empty String")
    case _ =>
      ()
