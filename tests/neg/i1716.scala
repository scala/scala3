object Fail {
  def f(m: Option[Int]): Unit = {
     m match {
      case x @ Some[_] =>       // error
      case _           =>
    }
  }
  Some[_]  // error
}
