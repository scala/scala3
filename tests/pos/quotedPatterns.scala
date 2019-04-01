object Test {

  val x = '{1 + 2}

  def f(x: Int) = x
  def g(x: Int, y: Int) = x * y

  val res: quoted.Expr[Int] = x match {
    case '{1 + 2} => '{0}
    case '{f($y)} => y
    case '{g($y, $z)} => '{$y * $z}
    case '{ 1 + ($y: Int)} => y
      // currently gives an unreachable case warning
      // but only when used in conjunction with the others.
      // I believe this is because implicit arguments are not taken
      // into account when checking whether we have already seen an `unapply` before.
    case _ => '{1}
  }
}