object tryTyping{
  def foo: Int = {
    try{???; 1}
    catch {
      case e: Exception => 2
    }
  }

  def foo2: Int = {
    val a2: (Throwable => Int) = _ match {case _ => 2}
    try{???; 1}
    catch a2
  }

  def foo3: Int = {
    val a3: (Int => Throwable => Int) = (b: Int) => _ match {case _ => b}
    try{???; 1}
    catch a3(3)
  }
}
