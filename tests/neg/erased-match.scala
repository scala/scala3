object Test {
  var b = true
  def foo erased (a: Int): Int = {
    a match { // error
      case _ =>
    }

    {
      println()
      a // error
    } match {
      case _ =>
    }

    b match {
      case true =>
        a // error
      case _ =>
        a // error
    }
  }
}
