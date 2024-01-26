object i19266:
  def fn1(x: Int, y: String = "y")(z: Double) = Some(s"$x$y$z")
  def fn2(x: Int)(y: String) = Some(s"$x$y")

  def checkWarning =
    fn1(1) // warn
    fn2(2) // warn
    val a = fn2(3) // warn
