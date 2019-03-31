object Test {

  val x = '{1 + 2}

  def f(x: Int) = x

  val res: quoted.Expr[Int] = x match {
    case '{1 + 2} => '{0}
    case '{f($y)} => y
    //case '{ 1 + ($y: Int)} => y  // currently gives an unreachable case error
    case _ => '{1}
  }
}