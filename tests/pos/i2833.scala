object a {
  val x: scala.Any =
    if (true)
      0L
    else if (false)
      (0: Int)
    else
      null
}
