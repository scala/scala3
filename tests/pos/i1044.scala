object Test {
  def f[X](x: X) =
    ???.getClass.getMethods.head.getParameterTypes.mkString(",")
      // this is the current behavior. But it would also be OK if `???.getClass` fails
    x.getClass.getMethods.head.getParameterTypes.mkString(",")
}
