object X {
  class Y
  def y = new Y {
    class Z
    def z = classOf[Z]
  }
}

object Test extends dotty.runtime.LegacyApp {
  assert(X.y.z.getEnclosingClass.getName == "X$$anon$1")
}
