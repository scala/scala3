object Fail {
  def f(m: Option[Int]): Unit = {
     m match {
      case Some[_] as x =>       // error: unbound wildcard type
      case _           =>
    }
  }
  Some[_]  // error: unbound wildcard type

}
