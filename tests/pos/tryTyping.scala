object tryTyping{
  def foo: Int = {
    try{???; 1}
    catch {
      case e: Exception => 2
    }
  }

  def foo2: Int = {
    val a: (Throwable => Int) = _ match {case _ => 2}
    try{???; 1}
    catch a
  }
}