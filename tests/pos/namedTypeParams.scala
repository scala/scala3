import language.experimental.namedTypeArguments

object Test {

  def f[X, Y](x: X, y: Y): Int = ???

  f[Int, String](1, "")
  f[X = Int, Y = String](1, "")
  f[X = Int](1, "")
  f[Y = String](1, "")
}

object TestInterleaving{
  def f2[X](using DummyImplicit)[Y](x: X, y: Y): Int = ???

  f2[X = Int][Y = String](1, "")
  f2[X = Int](1, "")


}
