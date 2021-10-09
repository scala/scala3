object Test {
  val x: Option[Unit => Int] = None
  x.getOrElse(() => 42)() // error: expression does not take more parameters
}
