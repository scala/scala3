object Test {
  def f[X](x: X) =
    x.getClass.getMethods.head.getParameterTypes.mkString(",")
}
