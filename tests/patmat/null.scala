class Test {
  def foo(x: Option[Int]) = x match {
    case Some(x) =>
    case None    =>
    case null    =>     // don't produce warning here
    case _       =>
  }

  def bar(x: Option[String]) = x match {
    case Some(a: String)        =>
    case Some(null)     =>
    case None           =>
    case y              =>
  }

  def quux(x: Option[String]) = x match {
    case Some(a)        =>
    case Some(null)     =>
    case None           =>
    case y              =>
    case _              =>
  }
}