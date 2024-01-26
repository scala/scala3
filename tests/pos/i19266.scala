object i19266:
  def fn1(x: Int, y: String = "y")(z: Double) = Some(s"$x$y$z")
  def fn2(x: Int)(y: String) = Some(s"$x$y")

  def checkCompile =
    fn1(1)
    // This should compile with warning
    fn2(2)
    val _ = fn2(3)
