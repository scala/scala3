object Test {

  val x = '{1 + 2}

  def f(x: Int) = x

  x match {
    case '{1 + 2} => 0
    case '{f($x)} => x
  }
}