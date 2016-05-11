object PatmatOrType {

  def foo1(x: Int | Double) = x match {
    case _: Int => true
    case _: Double => true
  }

  def foo2a(x: Int | Double | String) = x match { // _: String not matched
    case _: Int => true
    case _: Double => true
  }

  def foo2b(x: Int | Double | String) = x match {
    case _: Int => true
    case _: (Double | String) => true
  }

  def foo3(x: Option[Int | Double | String]) = x match { // warning: None, Some(_: String) not matched
    case Some(_: Int) => true
    case Some(_: Double) => true
  }

  def foo4(x: Option[Int | Double | String]) = x match {
    case Some(_: Int) => true
    case Some(_: Double) => true
    case Some(_: String) => true
    case None => false
  }

  def foo5a(x: Option[Int | Double | String]) = x match {
    case Some(_: (Int | Double)) => true
    case Some(_: String) => true
    case None => false
  }

  def foo5b(x: Option[Int | Double | String]) = x match { // warning: Some(_: String) not matched
    case Some(_: (Int | Double)) => true
    case None => false
  }
}