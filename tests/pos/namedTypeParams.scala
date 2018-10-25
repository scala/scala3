object Test {

  def f[X, Y](x: X, y: Y): Int = ???

  f[Int, String](1, "")
  f[X = Int, Y = String](1, "")
  f[X = Int](1, "")
  f[Y = String](1, "")

  f[X = Int][Y = String](1, "")
  f[X = Int][String](1, "")

  f[Y = String][X = Int](1, "")
  f[Y = String][Int](1, "")
}
