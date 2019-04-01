object Test {

  val x = '{1 + 2}

  def f(x: Int) = x
  def g(x: Int, y: Int) = x * y

  x match {
    case '{ val a = '{ println($y) }; 0 } => ??? // error: Not found: y
    case _ =>
  }
}