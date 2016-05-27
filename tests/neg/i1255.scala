object Test {
  def foo(x: Option[Int]) = x match {
    case Some(_: Double) => true // error
    case None => true
  }
}
